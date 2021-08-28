macro_rules! get_element {
    ($stream :expr, $err_fn: ident, $($pat:pat => $result:expr,)+) => {
       match ($stream).next()? {
            None => Err($err_fn(None)),
            Some(token) => match token.value {
                 $(
                     $pat => Ok($result),
                 )+
                 _ => Err($err_fn(Some(token))),
            }
        }
    };
}

macro_rules! get_optional_element {
    ($stream :expr, $($pat:pat => $result:expr,)+) => {
       match match ($stream).peek()? {
            None => None,
            Some(token) => match token.value {
                 $(
                     $pat => Some($result),
                 )+
                 _ => None,
            }
        }{
            None => None,
            Some(i) => {
                ($stream).consume()?;
                Some(i)
            }
        }
    };
}

macro_rules! get_optional_element_with_token {
    ($stream :expr, $($pat:pat => $result:expr,)+) => {
       match match ($stream).peek()? {
            None => None,
            Some(token) => match token.value {
                 $(
                     $pat => Some($result),
                 )+
                 _ => None,
            }
        }{
            None => None,
            Some(i) => {
                let token = ($stream).next()?.unwrap();
                Some((i, token))
            }
        }
    };
}
