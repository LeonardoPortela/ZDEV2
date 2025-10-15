
" Tratar texto de Observação para que não saia quebrado
*READ TABLE IT_VBAP INDEX 1.
*DO 20 TIMES.
*
*  APPEND IT_VBAP.
*ENDDO.
describe table  it_vbap lines wa_qntlinha.

clear: wa_linha.

data: wa_lines type ztline.

loop at it_lines into wa_lines.
  if wa_lines-tdformat ne 'N'.
    if sy-tabix eq 1.
      wa_linha = wa_lines-tdline.
    else.
      concatenate wa_linha wa_lines-tdline
      into wa_linha separated by space.
    endif.
  endif.
endloop.


