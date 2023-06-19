macro_rules! get_required_element {
    ($stream :expr, $expected: expr, $guidance: expr, $($pat:pat => $result:expr,)+) => {
       match ($stream).next()? {
            None => Err::<_, Box<error::Error>>(crate::parse::Error::new($stream.vm(), $expected, None, $guidance).into()),
            Some(token) => match token.value() {
                 $(
                     $pat => Ok($result),
                 )+
                 _ => Err(crate::parse::Error::new($stream.vm(), $expected, Some(token), $guidance).into()),
            }
        }
    };
}

macro_rules! get_optional_element {
    ($stream :expr, $($pat:pat => $result:expr,)+) => {
       match match ($stream).peek()? {
            None => None,
            Some(token) => match token.value() {
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
            Some(token) => match token.value() {
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
