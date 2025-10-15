*&---------------------------------------------------------------------*
*& Report  Z_DOC_FISCAL_AJUSTA_TP_EMISSAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_DOC_FISCAL_AJUSTA_TP_EMISSAO.

PARAMETERS: PDOCNUM TYPE J_1BDOCNUM.

START-OF-SELECTION.

  CALL FUNCTION 'Z_GRC_AJUSTA_TP_EMISSAO'
    EXPORTING
      I_DOCNUM          = PDOCNUM
    EXCEPTIONS
      NO_NUMBERING      = 1
      NOT_UNIQUE_SERVER = 2
      UPDATE_ERROR      = 3
      OTHERS            = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
