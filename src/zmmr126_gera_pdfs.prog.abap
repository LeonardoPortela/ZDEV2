*&---------------------------------------------------------------------*
*& Report  ZMMR126_GERA_PDFS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR126_GERA_PDFS.


PARAMETERS: P_URI TYPE STRING LOWER CASE,
            P_ARQ TYPE STRING LOWER CASE.

DATA: ARQUIVO       TYPE REF TO ZCL_ARQUIVO.

START-OF-SELECTION.

  CREATE OBJECT ARQUIVO.
  ARQUIVO->GET_FILE_URI_GET( EXPORTING I_URI = P_URI I_CONTENT_TYPE = '' IMPORTING E_TEXTO_2 = DATA(E_TEXTO_2)
    )->SET_FILE_READ( I_TEXTO_2 = E_TEXTO_2 I_NAME_ARQUIVO = P_ARQ
    )->GET_FILE_READ( EXPORTING I_NAME_ARQUIVO = P_ARQ IMPORTING E_TEXTO_X = DATA(E_TEXTO4) E_TEXTO = DATA(E_TEXTO3)
    ).

  BREAK-POINT.
