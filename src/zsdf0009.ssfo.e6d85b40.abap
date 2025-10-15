IF ( HEADER-empresa IS NOT INITIAL ).
SELECT SINGLE * into wA_zsdt0292 FROM zsdt0292
 WHERE empresa EQ HEADER-empresa AND status EQ 'A'.
if ( sy-subrc is initial ).
CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
  EXPORTING
    input         = wA_zsdt0292-cpf
 IMPORTING
   OUTPUT        = V_CPF_FORMATADO.
  endif.
  endif.
DATA: LV_LIFNR TYPE LIFNR.
LV_LIFNR = header-werks.
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
INPUT = LV_LIFNR
 IMPORTING
   OUTPUT        = LV_LIFNR.

select single * into wa_lfa1_werks from lfa1 where lifnr eq LV_LIFNR.
if ( sy-subrc is initial ).
CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
  EXPORTING
    input         = wa_lfa1_werks-STCD1
 IMPORTING
   OUTPUT        = v_lfa1_cnpf_formatado.
SELECT SINGLE * INTO WA_T005T FROM T005T WHERE LAND1 EQ wa_lfa1_werks-LAND1 AND SPRAS EQ SY-LANGU.
ENDIF.
















