package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"strings"
	"testing"
)

const texFileContent = `TeX File content \def\A{Hello}\A`

func TestRun(t *testing.T) {
	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		t.Fatalf("Unable to bind to a port: %s", err)
	}
	port := listener.Addr().(*net.TCPAddr).Port
	fmt.Println("Using port", port)
	go func() {
		run(listener, nil)
	}()

	req := struct {
		TexContent string
	}{
		TexContent: texFileContent,
	}
	b, _ := json.Marshal(&req)
	resp, err := http.Post(
		fmt.Sprintf("http://localhost:%d/share", port),
		"application/json",
		bytes.NewReader(b))
	if err != nil {
		t.Fatalf("Failed to POST to the server: %s", err)
	}
	b, err = io.ReadAll(resp.Body)
	if err != nil {
		resp.Body.Close()
		t.Fatalf("Failed to read the POST response: %s", err)
	}
	if err := resp.Body.Close(); err != nil {
		t.Fatalf("Failed to close the POST response: %s", err)
	}
	var response struct {
		Key string
	}
	if err := json.Unmarshal(b, &response); err != nil {
		t.Fatalf("Failed to parse the JSON response %s: %s", string(b), err)
	}
	fmt.Println("Share key is", response.Key)

	resp, err = http.Get(fmt.Sprintf("http://localhost:%d/s/%s", port, response.Key))
	if err != nil {
		t.Fatalf("Failed to GET to the server: %s", err)
	}
	b, err = io.ReadAll(resp.Body)
	if err != nil {
		resp.Body.Close()
		t.Fatalf("Failed to read the GET response: %s", err)
	}
	if err := resp.Body.Close(); err != nil {
		t.Fatalf("Failed to close the GET response: %s", err)
	}

	if !strings.Contains(string(b), texFileContent) {
		t.Fatalf("Response does not contain the TeX file content: %s", string(b))
	}
}

func TestRunFileTooBig(t *testing.T) {
	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		t.Fatalf("Unable to bind to a port: %s", err)
	}
	port := listener.Addr().(*net.TCPAddr).Port
	fmt.Println("Using port", port)
	go func() {
		run(listener, nil)
	}()

	req := struct {
		TexContent string
	}{
		TexContent: strings.Repeat("a", maxFileSize+1),
	}
	b, _ := json.Marshal(&req)
	resp, err := http.Post(
		fmt.Sprintf("http://localhost:%d/share", port),
		"application/json",
		bytes.NewReader(b))
	if err != nil {
		t.Fatalf("Failed to POST to the server: %s", err)
	}
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("Status codes not the same: %d != %d", resp.StatusCode, http.StatusBadRequest)
	}
	resp.Body.Close()
}

func TestCalculateHashUnderline(t *testing.T) {
	// The hash of "James" is k0WjWm_fF03_ before underline fixing
	if calculateHash("James") != "k0WjWm_fF03u" {
		t.Fatalf("%s != %s", calculateHash("James"), "k0WjWm_fF03u")
	}
}
