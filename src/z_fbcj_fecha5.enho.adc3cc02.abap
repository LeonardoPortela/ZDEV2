"Name: \FU:FCJ_CHECK_UNSAVED\SE:END\EI
ENHANCEMENT 0 Z_FBCJ_FECHA5.
*
***ITCJ_E_POSTINGS
*ITCJ_R_POSTINGS
*ITCJ_CR_POSTINGS
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Set 2021 - Leila - Inicio

** DEVK9A1IUK - FBCJ - Lanç ajustes ENHANCEMENT: Z_FBCJ_FECHA5 - BG #108901 inicio
  SELECT
    'I' AS sign,
    'EQ' AS option,
    valfrom AS low,
    valfrom AS high
 INTO TABLE @DATA(r_tpdoc)
 FROM setleaf
 WHERE setname = 'FBCJ_EXC_VALIDACAO'
 ORDER BY low ASCENDING.

LOOP AT ITCJ_E_POSTINGS ASSIGNING FIELD-SYMBOL(<fs_post_e>) WHERE document_status = 'S'
                                                                   and  POSTING_NUMBER  not in  r_tpdoc  . "- BG #108901
ENDLOOP.
IF sy-subrc = 0.


  e_answer = 'X'.
  vg_display_mens = 'X'.
 ENDIF.

LOOP AT itcj_r_postings ASSIGNING FIELD-SYMBOL(<fs_post_r>) WHERE document_status = 'S'
                                                             and  POSTING_NUMBER  not in  r_tpdoc . "- BG #108901
ENDLOOP.
IF sy-subrc = 0.
   e_answer = 'X'.
  vg_display_mens = 'X'.
 ENDIF.

LOOP AT itcj_cr_postings ASSIGNING FIELD-SYMBOL(<fs_post_cr>) WHERE document_status = 'S'
                                                               and  POSTING_NUMBER  not in  r_tpdoc . "- BG #108901
ENDLOOP.
IF sy-subrc = 0.
   e_answer = 'X'.
  vg_display_mens = 'X'.
 ENDIF.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Set 2021 - Leila - Fim
ENDENHANCEMENT.
