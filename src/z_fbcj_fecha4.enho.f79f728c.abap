"Name: \FU:FCJ_POST_ALL\SE:BEGIN\EI
ENHANCEMENT 0 Z_FBCJ_FECHA4.
*
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Set 2021 - Leila - Inicio
  CALL FUNCTION 'ZFI_DIVISAO_CAIXA'
    TABLES
      itcj_postings                  = ITCJ_POSTINGS
   EXCEPTIONS
     CAIXA_DIVISAO_NAO_EXISTE       = 1
     SET_NAO_EXIT                   = 2
     OTHERS                         = 3.

IF sy-subrc <> 0.
         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
***/// CS2021000281 - FBCJ - Estorno documentos não autorizados - Set 2021 - Leila - Fim
ENDENHANCEMENT.
