#include <hammer/hammer.h>

const HParser* init_parser(void) {
  HParser* digit = h_ch_range('0', '9');
  HParser* letter = h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
  HParser* number = h_many1(digit);
  HParser* ident =
      h_sequence(letter, h_many(h_choice(letter, digit, NULL)), NULL);
  HParser* ws = h_many(h_choice(h_ch(' '),
      h_ch('\t'),
      h_ch('\r'),
      h_ch('\n'),
      h_ch('\f'),
      h_ch('\v'),
      NULL));
  HParser* period = h_ch('.');
  HParser* kw_const = h_token((uint8_t[]){'c', 'o', 'n', 's', 't'}, 5);
  HParser* const_decl = h_sequence(ident, ws, h_ch('='), ws, number, NULL);
  HParser* const_block = h_sequence(kw_const,
      ws,
      const_decl,
      ws,
      h_many(h_sequence(h_ch(','), ws, const_decl, ws, NULL)),
      ws,
      h_ch(';'),
      NULL);
  HParser* kw_var = h_token((uint8_t[]){'v', 'a', 'r'}, 3);
  HParser* var_block = h_sequence(kw_var,
      ws,
      ident,
      ws,
      h_many(h_sequence(h_ch(','), ws, ident, ws, NULL)),
      ws,
      h_ch(';'),
      NULL);
  HParser* kw_procedure =
      h_token((uint8_t[]){'p', 'r', 'o', 'c', 'e', 'd', 'u', 'r', 'e'}, 9);
  HParser* block_indirect = h_indirect();
  HParser* procedure_decl = h_sequence(kw_procedure,
      ws,
      ident,
      ws,
      h_ch(';'),
      ws,
      block_indirect,
      ws,
      h_ch(';'),
      NULL);
  HParser* colon_equals = h_token((uint8_t[]){':', '='}, 2);
  HParser* plus_minus = h_choice(h_ch('+'), h_ch('-'), NULL);
  HParser* expression_indirect = h_indirect();
  HParser* factor = h_choice(ident,
      number,
      h_sequence(h_ch('('), ws, expression_indirect, ws, h_ch(')'), NULL),
      NULL);
  HParser* star_slash = h_choice(h_ch('*'), h_ch('/'), NULL);
  HParser* term = h_sequence(
      factor, ws, h_many(h_sequence(star_slash, ws, factor, ws, NULL)), NULL);
  HParser* expression = h_sequence(plus_minus,
      ws,
      term,
      ws,
      h_many(h_sequence(plus_minus, ws, term, ws, NULL)),
      NULL);
  h_bind_indirect(expression_indirect, expression);
  HParser* kw_odd = h_token((uint8_t[]){'o', 'd', 'd'}, 3);
  HParser* odd_cond = h_sequence(kw_odd, ws, expression, NULL);
  HParser* leq = h_token((uint8_t[]){'<', '='}, 2);
  HParser* geq = h_token((uint8_t[]){'>', '='}, 2);
  HParser* comparison_op =
      h_choice(h_ch('='), h_ch('#'), leq, geq, h_ch('<'), h_ch('>'), NULL);
  HParser* comparison_cond =
      h_sequence(expression, ws, comparison_op, ws, expression, NULL);
  HParser* condition = h_choice(odd_cond, comparison_cond, NULL);
  HParser* assignment_st =
      h_sequence(ident, ws, colon_equals, ws, expression, NULL);
  HParser* kw_call = h_token((uint8_t[]){'c', 'a', 'l', 'l'}, 4);
  HParser* procedure_call_st = h_sequence(kw_call, ws, ident, NULL);
  HParser* read_st = h_sequence(h_ch('?'), ws, ident, NULL);
  HParser* write_st = h_sequence(h_ch('!'), ws, expression, NULL);
  HParser* kw_begin = h_token((uint8_t[]){'b', 'e', 'g', 'i', 'n'}, 5);
  HParser* kw_end = h_token((uint8_t[]){'e', 'n', 'd'}, 3);
  HParser* statement_indirect = h_indirect();
  HParser* block_st = h_sequence(kw_begin,
      ws,
      statement_indirect,
      ws,
      h_many(h_sequence(h_ch(';'), ws, statement_indirect, ws, NULL)),
      ws,
      kw_end,
      NULL);
  HParser* kw_if = h_token((uint8_t[]){'i', 'f'}, 2);
  HParser* kw_then = h_token((uint8_t[]){'t', 'h', 'e', 'n'}, 4);
  HParser* if_st = h_sequence(
      kw_if, ws, condition, ws, kw_then, ws, statement_indirect, NULL);
  HParser* kw_while = h_token((uint8_t[]){'w', 'h', 'i', 'l', 'e'}, 5);
  HParser* kw_do = h_token((uint8_t[]){'d', 'o'}, 2);
  HParser* while_st = h_sequence(
      kw_while, ws, condition, ws, kw_do, ws, statement_indirect, NULL);
  HParser* statement = h_optional(h_choice(assignment_st,
      procedure_call_st,
      read_st,
      write_st,
      block_st,
      if_st,
      while_st,
      NULL));
  h_bind_indirect(statement_indirect, statement);
  HParser* block = h_sequence(h_optional(const_block),
      ws,
      h_optional(var_block),
      ws,
      h_many(h_sequence(procedure_decl, ws, NULL)),
      ws,
      statement,
      NULL);
  h_bind_indirect(block_indirect, block);
  HParser* program = h_sequence(block, ws, h_ch('.'), ws, h_end_p(), NULL);
  return program;
}

int main(void) {
  const HParser* program = init_parser();
}
