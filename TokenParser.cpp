#include "TokenParser.hpp"

namespace satisfy {

std::mutex GlobalTokenParser::read_mutex_;

int
GlobalTokenParser::getNextChar(void) noexcept {
  std::lock_guard<std::mutex> _(read_mutex_);
  return std::getchar();
}

bool
GlobalTokenParser::isWhitespace(int p) noexcept {
  if(p == '\n') {
    file_pos_.updateCol();
    return true;
  }
  file_pos_.updateRow();
  return std::isspace(p);
}

TokenType
GlobalTokenParser::parseToken(void) noexcept {
  static int lch = (int)' ';
  static auto _update_lch =
      [](Pos & _fp) -> int {
        _fp.updateRow();
        return lch = getNextChar();
      };

  auto update_lch = std::bind(_update_lch, file_pos_);
  
  static auto _parse_number =
      [](auto & upd_lch, auto & lstring) -> void {
        for(; std::isdigit(upd_lch());
            lstring += lch);
        if(lch == '.')
          for(lstring += '.';
              std::isdigit(upd_lch());
              lstring += lch);
      };

  // auto parse_number = std::bind(_parse_number, update_lch, last_token_string_);
  auto parse_number = [&]() -> void {
    _parse_number(update_lch, last_token_string_);
  };

  while(isWhitespace(lch))
    update_lch();

  if(lch == EOF) {
    // update_lch();
    return returnToken(TokenType::tokenEOF);
  }

  if(lch == '#') {
    for(this->file_pos_.updateCol(); getNextChar() != '\n';);
    // eat '\n'
    lch = getNextChar();
    return returnToken(parseToken());
  }

  if(lch == ':') {
    if(update_lch() == ':') {
      this->last_token_string_ = "::";
      update_lch();
    } else
      this->last_token_string_ = ":";
    return returnToken(TokenType::DeclType);
  }

  if(lch == '/') {
    this->last_token_string_ = (char)lch;
    return returnToken(TokenType::Seperator);
  }

  if(lch == '-') {
    if(std::isdigit(update_lch())) {
      parse_number();
      return returnToken(TokenType::Number);
    }
    return returnToken(TokenType::Operator);
  }

  if(lch == '+'/* || lch == '-'*/ ||
     lch == '*'/* || lch == '|'*/) {
    this->last_token_string_ = (char)lch;
    update_lch();
    return returnToken(TokenType::Operator);
  }

  if(lch == '&') {
    if(update_lch() == '&') {
      this->last_token_string_ = "&&";
      update_lch();
      return returnToken(TokenType::Operator);
    }
    // skip, IDK
    lch = getNextChar();
    return returnToken(parseToken());
  }

  if(lch == '|') {
    if(update_lch() == '|') {
      this->last_token_string_ = "||";
      update_lch();
    } else
      this->last_token_string_ = "|";
    return returnToken(TokenType::Operator);
  }

  // xor
  if(lch == '^') {
    if(update_lch() == '^') {
      this->last_token_string_ = "^^";
      update_lch();
      return returnToken(TokenType::Operator);
    }
    // skip
    update_lch();
    return returnToken(parseToken());
  }

  if(lch == '!') {
    if(update_lch() == '!') {
      this->last_token_string_ = "!!";
      update_lch();
      return returnToken(TokenType::Operator);
    }
    this->last_token_string_ = "!";
    return returnToken(TokenType::Supporter);
  }

  if(std::isdigit(lch)) {
    parse_number();
    
    return returnToken(TokenType::Number);
  }

  for(this->last_token_string_.clear(); isAlphabetic(lch); update_lch())
    this->last_token_string_ += (char)lch;

  if(!this->last_token_string_.empty()) {

    if(last_token_string_ == "cst" || // const
       last_token_string_ == "fnl" || // final
       last_token_string_ == "cls" || // class
       last_token_string_ == "const" || // constructor
       last_token_string_ == "dest") // destructor
      return returnToken(TokenType::DeclSupporter);

    if(last_token_string_ == "imp")
      return returnToken(TokenType::ImportDirective);

    return returnToken(TokenType::Identifier);
  }

  return returnToken(TokenType::tokenEOF);
}

TokenType
GlobalTokenParser::getLastToken(void) noexcept {
  return this->last_token_type_;
}

std::string
GlobalTokenParser::getIdentifierStr(void) noexcept {
  return this->last_token_string_;
}

int GlobalTokenParser::asI64(void) noexcept {
  return std::stoi(this->last_token_string_);
}

unsigned GlobalTokenParser::asU64(void) noexcept {
  return std::stoul(this->last_token_string_);
}

double GlobalTokenParser::asF64(void) noexcept {
  return std::stod(this->last_token_string_);
}

Pos GlobalTokenParser::getPos(void) noexcept {
  return this->file_pos_;
}

} // ns satisfy
