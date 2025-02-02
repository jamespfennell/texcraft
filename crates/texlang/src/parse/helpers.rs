macro_rules! get_required_element {
    ($stream :expr, $expected: expr, $guidance: expr, $($pat:pat => $result:expr,)+) => {
        // TODO: probably should call get_optional_element_with_token
        match ($stream).peek()?.copied() {
            Some(token) => match token.value() {
                 $(
                     $pat => {
                        ($stream).consume()?;
                        Some($result)
                     },
                 )+
                 _ => {
                    $stream.vm().error(crate::parse::Error::new($expected, Some(token), $guidance))?;
                    None
                 },
            }
            None => {
                $stream.vm().error(crate::parse::Error::new($expected, None, $guidance))?;
                None
            },
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
