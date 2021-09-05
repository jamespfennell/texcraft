package main

import (
	"context"
	"crypto/sha256"
	"embed"
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"mime"
	"net"
	"net/http"
	"os"
	"path"
	"strings"
	"sync"
	"time"

	"github.com/go-redis/redis/v8"
)

// The generated UI files are embedded in the binary.
//go:embed ui/dist/*
var uiFS embed.FS

// We embed the index.html manually to make it a compile error if this file is missing
//go:embed ui/dist/index.html
var indexDotHtml string

const maxFileSize = 1024 * 10

var flagPort = flag.Int("port", 8080, "The port to bind to")
var flagInMemory = flag.Bool("memory", false, "Store shared files in memory")
var flagRedis = flag.String("redis", "", "Store shared files in the Redis instance at the provided address")

func main() {
	flag.Parse()
	var redisAddr *string
	if !*flagInMemory {
		if *flagRedis == "" {
			fmt.Println("Error: must either specify --memory or a Redis address")
			os.Exit(1)
		}
		redisAddr = flagRedis
	}
	listener, err := net.Listen("tcp", fmt.Sprintf(":%d", *flagPort))
	if err != nil {
		fmt.Printf("Unable to bind to port %d\n", *flagPort)
		os.Exit(1)
	}
	fmt.Printf("Listing on port %d\n", *flagPort)
	run(listener, redisAddr)
}

func run(listener net.Listener, redisAddr *string) {
	fmt.Println("Launching")
	var db DB
	if redisAddr != nil {
		db = (*RedisDB)(redis.NewClient(&redis.Options{
			Addr:     *redisAddr,
			Password: "", // no password set
			DB:       0,  // use default DB
		}))
	} else {
		db = &InMemoryDB{
			data: map[string]SharedFile{},
		}
	}

	mux := http.NewServeMux()

	uiFiles, _ := uiFS.ReadDir("ui/dist")
	for _, uiFile := range uiFiles {
		uiFile := uiFile
		mux.HandleFunc("/"+uiFile.Name(), func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", mime.TypeByExtension(path.Ext(uiFile.Name())))
			b, _ := uiFS.Open("ui/dist" + r.URL.Path)
			io.Copy(w, b)
		})
		fmt.Printf("Registered static file %s\n", uiFile.Name())
	}
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			writeHtmlFile(w, fmt.Sprintf("Could not find file %s.", r.URL.Path),
				"404.tex", http.StatusNotFound)
			return
		}
		writeHtmlFile(w, "", "", http.StatusOK)
	})

	mux.HandleFunc("/share", func(w http.ResponseWriter, r *http.Request) {
		type CreateSharedFileRequest struct {
			TexContent string
		}
		type CreateSharedFileResponse struct {
			Key string
		}
		if r.Method != "POST" {
			writeError(w, "Only POST requests are allowed for /share", http.StatusMethodNotAllowed)
			return
		}
		body, err := io.ReadAll(r.Body)
		if err != nil {
			writeError(w, "Failed to read message body", http.StatusInternalServerError)
			return
		}
		var request CreateSharedFileRequest
		if err := json.Unmarshal(body, &request); err != nil {
			writeError(w, "Request body must be JSON of the form {\"TexContent\": \"...\"}", http.StatusBadRequest)
			return
		}
		request.TexContent = strings.TrimSpace(request.TexContent)
		if len(request.TexContent) > maxFileSize {
			writeError(w, "TeX file too big: must be under 10kb", http.StatusBadRequest)
			return
		}
		key := calculateHash(request.TexContent)
		db.Set(key, SharedFile{
			TexContent: request.TexContent,
			Created:    time.Now().UTC(),
			LastViewed: time.Now().UTC(),
			NumViews:   0,
		})
		response := CreateSharedFileResponse{
			Key: key,
		}
		b, _ := json.Marshal(&response)
		w.Header().Set("Content-Type", mime.TypeByExtension(".json"))
		_, _ = io.WriteString(w, string(b))
	})

	mux.HandleFunc("/s/", func(w http.ResponseWriter, r *http.Request) {
		key := strings.TrimPrefix(r.URL.Path, "/s/")
		file, ok := db.Get(key)
		if !ok {
			writeHtmlFile(w, fmt.Sprintf("Could not find shared Tex file with key %q.", key),
				"404.tex", http.StatusNotFound)
			return
		}
		writeHtmlFile(w, file.TexContent, "input.tex", http.StatusOK)
	})
	srv := &http.Server{Handler: mux}
	srv.Serve(listener)
}

func writeHtmlFile(w http.ResponseWriter, initialTex string, initialFilename string, statusCode int) {
	w.WriteHeader(statusCode)
	w.Header().Set("Content-Type", mime.TypeByExtension(".html"))
	html := indexDotHtml
	if initialTex != "" {
		initialTex = strings.ReplaceAll(initialTex, `"`, `&quot;`)
		html = strings.Replace(indexDotHtml, `data-initial-tex=""`, fmt.Sprintf(`data-initial-tex="%s&#13;&#10;"`, initialTex), 1)
	}
	if initialFilename != "" {
		html = strings.Replace(html, `data-initial-filename=""`, fmt.Sprintf(`data-initial-filename="%s"`, initialFilename), 1)
	}
	io.WriteString(w, html)
}

func writeError(w http.ResponseWriter, message string, statusCode int) {
	w.WriteHeader(statusCode)
	w.Header().Set("Content-Type", mime.TypeByExtension(".json"))
	response := struct {
		ErrorMessage string
	}{
		ErrorMessage: message,
	}
	b, _ := json.Marshal(&response)
	w.Write(b)
}

func calculateHash(s string) string {
	h := sha256.New()
	io.WriteString(h, s)
	sum := h.Sum(nil)
	b := make([]byte, base64.URLEncoding.EncodedLen(len(sum)))
	base64.URLEncoding.Encode(b, sum)
	if b[11] == '_' {
		b[11] = 'u'
	}
	return string(b)[:12]
}

type SharedFile struct {
	TexContent string
	Created    time.Time
	LastViewed time.Time
	NumViews   int64
}

type DB interface {
	Get(string) (SharedFile, bool)
	Set(string, SharedFile)
}

type InMemoryDB struct {
	data  map[string]SharedFile
	mutex sync.Mutex
}

func (db *InMemoryDB) Set(s string, file SharedFile) {
	db.mutex.Lock()
	defer db.mutex.Unlock()
	db.data[s] = file
}

func (db *InMemoryDB) Get(s string) (SharedFile, bool) {
	db.mutex.Lock()
	defer db.mutex.Unlock()
	file, ok := db.data[s]
	return file, ok
}

type RedisDB redis.Client

func (db *RedisDB) Set(s string, file SharedFile) {
	b, _ := json.Marshal(&file)
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	((*redis.Client)(db)).Set(ctx, s, string(b), 0)
}

func (db *RedisDB) Get(s string) (SharedFile, bool) {
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	val, err := ((*redis.Client)(db)).Get(ctx, s).Result()
	if err != nil {
		return SharedFile{}, false
	}
	var file SharedFile
	if err := json.Unmarshal([]byte(val), &file); err != nil {
		fmt.Printf("Failed to unmarshal key %s from Redis: %s\n", s, err)
		return SharedFile{}, false
	}
	return file, true
}
