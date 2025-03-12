macro_rules! get_required_element {
    ($stream :expr, $expected: expr, $guidance: expr, $($pat:pat => $result:expr,)+) => {
        // TODO: probably should call get_optional_element_with_token
        match ($stream).next()? {
            Some(token) => match token.value() {
                 $(
                     $pat => {
                        Some($result)
                     },
                 )+
                 _ => {
                    $stream.error(crate::parse::Error::new($expected, Some(token), $guidance))?;
                    $stream.back(token);
                    None
                 },
            }
            None => {
                $stream.error(crate::parse::Error::new($expected, None, $guidance))?;
                None
            },
        }
    };
}

macro_rules! get_optional_element {
    ($stream :expr, $($pat:pat => $result:expr,)+) => {
        match ($stream).next()? {
            None => None,
            Some(token) => match token.value() {
                 $(
                     $pat => Some($result),
                 )+
                 _ => {
                    $stream.back(token);
                    None
                 }
            }
        }
    };
}

macro_rules! get_optional_element_with_token {
    ($stream :expr, $($pat:pat => $result:expr,)+) => {
       match ($stream).next()? {
            None => None,
            Some(token) => match token.value() {
                 $(
                     $pat => {
                        Some(($result, token))
                    },
                 )+
                 _ => {
                    $stream.back(token);
                    None
                 },
            }
        }
    };
}
