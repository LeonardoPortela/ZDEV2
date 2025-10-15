FUNCTION Z_ISP_CONVERT_FIRSTCHARS_TOUPP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(INPUT_STRING) TYPE  C
*"     VALUE(SEPARATORS) TYPE  C DEFAULT ' -.,;:'
*"  EXPORTING
*"     VALUE(OUTPUT_STRING) TYPE  C
*"----------------------------------------------------------------------
  data: pos     like sy-fdpos,
        pos_max like sy-fdpos.
  field-symbols: <poi>, <hpoi>, <rest>.
*
  check not input_string is initial.
*
  call function 'Z_ISM_CONVERT_TO_NORMAL_FORM'
       exporting
            input  = input_string
       importing
            output = input_string.
  output_string = input_string.

  translate output_string to lower case.                  "#EC SYNTCHAR
*
  pos_max = strlen( output_string ) - 1.
*
  pos = 0.
  assign output_string+pos(1) to <poi>.
  assign input_string+pos(1)  to <hpoi>.
  <poi> = <hpoi>.
*
  assign input_string+pos(*) to <rest>.
  while <rest> ca separators.
    pos = pos + sy-fdpos + 1.
    if pos > pos_max. exit. endif.
    assign output_string+pos(1) to <poi>.
    assign input_string+pos(1)  to <hpoi>.
    <poi> = <hpoi>.
    assign input_string+pos(*) to <rest>.
  endwhile.
*




ENDFUNCTION.
