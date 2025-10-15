FUNCTION z_sd_print_declara.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DOC_NUMERO) TYPE  J_1BDOCNUM
*"     REFERENCE(IMPRIMIR) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(GERA_PDF) TYPE  CHAR01 DEFAULT 'N'
*"  EXPORTING
*"     VALUE(E_DECLARACAO) TYPE  XSTRING
*"  CHANGING
*"     VALUE(T_PDF) TYPE  ZLESE0034 OPTIONAL
*"----------------------------------------------------------------------

  DATA: wl_zlest0061  TYPE zlest0061,
        wa_nfe_active TYPE j_1bnfe_active.

  DATA: lt_pdf TYPE TABLE OF zlese0034.

*#127471-18.04.2024-JT-inicio - comentado
*---------------------------------------
* nao imprime mais declaracao motorista
*---------------------------------------
  EXIT.
*#127471-18.04.2024-JT-fim - comentado

  t_doc_numero = doc_numero.

  SELECT SINGLE *
    FROM j_1bnfe_active
    INTO wa_nfe_active
   WHERE docnum    =  doc_numero
   AND docsta   = '1'.

  SELECT SINGLE *
    FROM zlest0061
    INTO wl_zlest0061
    WHERE docnum = doc_numero.

  IF sy-subrc NE 0 AND wa_nfe_active-model = '57'. "CTE
*    PERFORM F_IMPRIME_DECLARA
*            USING DOC_NUMERO
*                  IMPRIMIR
*            CHANGING lc_DECLARACAO
*                     lt_PDF.



    PERFORM f_imprime_declara
                USING
                   doc_numero
                   imprimir
                CHANGING
                   e_declaracao
                   t_pdf.

  ENDIF.

ENDFUNCTION.
