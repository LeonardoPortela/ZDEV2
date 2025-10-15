FUNCTION zmm_check_mensagem_bloqueio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ID) TYPE  SYMSGID
*"     REFERENCE(NUMBER) TYPE  SYMSGNO
*"  EXPORTING
*"     VALUE(IS_BLOCK) TYPE  CHAR1
*"----------------------------------------------------------------------

  DATA: t_value TYPE rgsb4 OCCURS 0 WITH HEADER LINE.

  CLEAR is_block.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'MSGBLOCKUSER'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  CHECK sy-subrc IS INITIAL.
  CHECK t_value IS NOT INITIAL.

  DATA(mensagem) = |{ id }{ number }|.

  CHECK line_exists( t_value[ from = mensagem ] ).

  is_block = abap_true.

ENDFUNCTION.
