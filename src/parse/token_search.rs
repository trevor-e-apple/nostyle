use crate::tokenize::tokens::{Token, Tokens};

/// finds the index of the matching rtoken for the first ltoken found at
/// starts_at
pub fn find_matching_group_indices(
    tokens: &Tokens,
    ltoken: &Token,
    rtoken: &Token,
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    let mut ltokens_found = 1;
    let mut rtokens_found = 0;

    for (displacement, token) in
        tokens.token_slice(starts_at + 1, ends_at).into_iter().enumerate()
    {
        if *token == *ltoken {
            ltokens_found += 1;
        } else if *token == *rtoken {
            rtokens_found += 1;
        }

        if ltokens_found == rtokens_found {
            return Some(starts_at + 1 + displacement);
        }
    }

    None
}

/// finds the index of the matching ltoken for the rtoken found at
/// ends_at
pub fn find_matching_group_indices_end(
    tokens: &Tokens,
    ltoken: &Token,
    rtoken: &Token,
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    let mut ltokens_found = 0;
    let mut rtokens_found = 1;

    for (displacement, token) in
        tokens.token_slice(starts_at, ends_at - 1).into_iter().enumerate().rev()
    {
        if *token == *ltoken {
            ltokens_found += 1;
        } else if *token == *rtoken {
            rtokens_found += 1;
        }

        if ltokens_found == rtokens_found {
            return Some(starts_at + displacement);
        }
    }

    None
}

struct AllGroupsSearch {
    brace_level: i32,
    paren_level: i32,
}

impl AllGroupsSearch {
    fn new() -> Self {
        Self { brace_level: 0, paren_level: 0 }
    }

    /// checks check_token to see if the group levels need modification
    fn check_for_group_tokens(&mut self, check_token: &Token) {
        if *check_token == Token::LBrace {
            self.brace_level += 1;
        } else if *check_token == Token::RBrace {
            self.brace_level -= 1;
        } else if *check_token == Token::LParen {
            self.paren_level += 1;
        } else if *check_token == Token::RParen {
            self.paren_level -= 1;
        }
    }

    fn at_starting_level(&self) -> bool {
        self.paren_level == 0 && self.brace_level == 0
    }

    fn has_escaped(&self) -> bool {
        self.paren_level < 0 || self.brace_level < 0
    }
}

/// Finds the next matching token and ignores both braces and parentheses group markers.
///
/// Returns a tuple of the index that token that was found
/// Returns None if not found
pub fn find_final_matching_level_token_all_groups(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
) -> Option<(usize, Token)> {
    let mut result: Option<(usize, Token)> = None;

    let mut group_levels = AllGroupsSearch::new();
    for (displacement, check_token) in
        tokens.token_slice(starts_at, ends_at).into_iter().enumerate()
    {
        group_levels.check_for_group_tokens(check_token);

        if group_levels.at_starting_level()
            && matching_tokens.contains(check_token)
        {
            result = Some((starts_at + displacement, check_token.clone()));
        }

        if group_levels.has_escaped() {
            // we have moved outside of the grouping that starts_at was in
            return None;
        }
    }

    result
}

/// finds the index of the next token after starts_at and before ends_at
/// (starts_at <= index < ends_at) that is at the same grouping level as
/// starts_at
///
/// returns None if not found
pub fn find_next_matching_level_token(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
    group_start_token: &Token,
    group_end_token: &Token,
) -> Option<usize> {
    let mut current_level = 0;

    for (displacement, check_token) in
        tokens.token_slice(starts_at, ends_at).into_iter().enumerate()
    {
        if current_level == 0 && matching_tokens.contains(check_token) {
            return Some(starts_at + displacement);
        } else if *check_token == *group_start_token {
            current_level += 1;
        } else if *check_token == *group_end_token {
            current_level -= 1;
        }
    }

    None
}

/// finds the index of the next token after starts_at and before ends_at
/// (starts_at <= index < ends_at) that is at the same grouping level as
/// starts_at
///
/// returns None if not found
pub fn find_next_matching_level_token_all_groups(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    let mut group_levels = AllGroupsSearch::new();

    for (displacement, check_token) in
        tokens.token_slice(starts_at, ends_at).into_iter().enumerate()
    {
        if group_levels.at_starting_level()
            && matching_tokens.contains(check_token)
        {
            return Some(starts_at + displacement);
        } else {
            group_levels.check_for_group_tokens(check_token);
        }
    }

    None
}

/// finds the index of the token previous token before ends_at and after
/// starts_at (starts_at <= index < ends_at) that is at the same grouping level
/// as ends_at
///
/// returns None if not found
pub fn find_prev_matching_level_token_all_groups(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    let mut group_levels = AllGroupsSearch::new();

    for index in (starts_at..ends_at).rev() {
        if let Some(check_token) = tokens.get_token(index) {
            if group_levels.at_starting_level()
                && matching_tokens.contains(check_token)
            {
                return Some(index);
            } else {
                group_levels.check_for_group_tokens(check_token);
            }
        } else {
            return None;
        }
    }

    None
}

/// finds the index of the next token of a certain type
pub fn find_next_token(
    tokens: &Tokens,
    token: &Token,
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    for index in starts_at..ends_at {
        if let Some(check_token) = tokens.get_token(index) {
            if *check_token == *token {
                return Some(index);
            }
        }
    }

    None
}

/// finds the index of the next token among a few types
pub fn find_next_tokens(
    tokens: &Tokens,
    matching_tokens: &[Token],
    starts_at: usize,
    ends_at: usize,
) -> Option<usize> {
    for index in starts_at..ends_at {
        if let Some(check_token) = tokens.get_token(index) {
            if matching_tokens.contains(check_token) {
                return Some(index);
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::{tokenize, tokens::Token};

    #[test]
    fn mismatched_braces_find_from_end() {
        let tokens = tokenize("{}}").expect("Unexpected tokenize error");
        match find_matching_group_indices_end(
            &tokens,
            &Token::LBrace,
            &Token::RBrace,
            0,
            tokens.len(),
        ) {
            Some(_) => assert!(false),
            None => {}
        }
    }
}
