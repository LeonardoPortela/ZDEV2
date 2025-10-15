"Name: \PR:RFFOBR_D\FO:FILL_DETAILS_ITAU\SE:END\EI
ENHANCEMENT 0 Z_ITAU_MULTA.

DATA: lva_num1(10) TYPE c,
      lva_deci(02) TYPE c,
      lv_m04       TYPE n LENGTH 13,
      lv_m06       TYPE n LENGTH 6.

cnt_records = cnt_records + 1.

CLEAR zfi_j_1bdmeym.
zfi_j_1bdmeym-m01 = '2'.
zfi_j_1bdmeym-m02 = '2'.

CONCATENATE hlp_duedate+6(2) hlp_duedate+4(2) hlp_duedate+0(4) INTO zfi_j_1bdmeym-m03.

CONDENSE zfi_j_1bdmeym-m03 NO-GAPS.

*Inicio Alteração - Leandro Valentim Ferreira - 04.07.23 - #115610
CLEAR: vl_valor, vl_data.
CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
  EXPORTING
    i_belnr = regup-belnr
    i_bukrs = regup-bukrs
  IMPORTING
    e_multa = vl_valor.

MOVE vl_valor TO zfi_j_1bdmeym-m04.

TRANSLATE zfi_j_1bdmeym-m04 USING '. '.
CONDENSE zfi_j_1bdmeym-m04 NO-GAPS.

lv_m04 = zfi_j_1bdmeym-m04.
zfi_j_1bdmeym-m04 = lv_m04.

lv_m06 = cnt_records.
zfi_j_1bdmeym-m06 = lv_m06.

ENDENHANCEMENT.
