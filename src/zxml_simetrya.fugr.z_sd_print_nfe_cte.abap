FUNCTION z_sd_print_nfe_cte.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(DOC_NUMERO) TYPE  J_1BDOCNUM
*"     REFERENCE(IMPRIMIR) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(GERA_PDF) TYPE  CHAR1 DEFAULT 'N'
*"  CHANGING
*"     VALUE(AUTHCOD_OUT) TYPE  J_1BNFEAUTHCODE OPTIONAL
*"     VALUE(CODE_OUT) TYPE  J_1BSTATUSCODE OPTIONAL
*"     VALUE(DOCNUM9_OUT) TYPE  J_1BDOCNUM9 OPTIONAL
*"     VALUE(CDV_OUT) TYPE  J_1BCHECKDIGIT OPTIONAL
*"     VALUE(DS_URL_DANFE_OUT) TYPE  CHAR100 OPTIONAL
*"  EXCEPTIONS
*"      NAO_LOCALIZADO
*"----------------------------------------------------------------------


  DATA: url       TYPE zib_nfe,
        url_prgn  TYPE agr_url,
        lva_hbkid TYPE t012-hbkid. "

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  DATA: gv_par    TYPE syst_subrc,
        gv_screen TYPE char1,
        lc_nast   TYPE nast,
        it_otf    TYPE tsfotf.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

  IF imprimir IS INITIAL.

    SELECT SINGLE * INTO url
      FROM zib_nfe
     WHERE docnum EQ doc_numero.

  ELSE.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    SELECT SINGLE contingencia
      INTO @DATA(lv_contingencia)
      FROM zsdt0102
     WHERE docnum = @doc_numero.

    IF sy-subrc = 0 AND lv_contingencia = abap_true.
      lc_nast-objky   = doc_numero.
      lc_nast-tdarmod = '9'.
      PERFORM entry2 IN PROGRAM zbrmdfe_damdfe_nast USING gv_par gv_screen lc_nast CHANGING it_otf.
      sy-subrc = 0.
    ELSE.
      SELECT SINGLE * INTO url
        FROM zib_nfe
       WHERE docnum EQ doc_numero
         AND ds_url_danfe NE space.
    ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
  ENDIF.

  IF sy-subrc EQ 0.

    authcod_out      = url-authcod.
    code_out         = url-code.
    docnum9_out      = url-docnum9.
    cdv_out          = url-cdv.
    ds_url_danfe_out = url-ds_url_danfe.

    url_prgn = url-ds_url_danfe.
    "ALRS
    IF imprimir IS NOT INITIAL.

      CALL FUNCTION 'Z_SD_PRINT_DECLARA'
        EXPORTING
          doc_numero     = doc_numero
        EXCEPTIONS
          nao_localizado = 1
          OTHERS         = 2.

*** BUG - 84932 - Inicio - CBRAND
      CALL FUNCTION 'Z_SD_BUSCA_HBKID'
        EXPORTING
          doc_numero = doc_numero
        IMPORTING
          hbkid      = lva_hbkid.
*** BUG - 84932 - Fim - CBRAND


      CALL FUNCTION 'Z_SD_PRINT_BOLETO'
        EXPORTING
          doc_numero     = doc_numero
          tipo           = 'N'  "NOTA
          hbkid          = lva_hbkid
        EXCEPTIONS
          nao_localizado = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        IF gera_pdf NE 'S' AND lv_contingencia = abap_false.
          CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
            EXPORTING
              node_data = url_prgn.
          IF url-ds_url_cle IS NOT INITIAL.
            url_prgn = url-ds_url_cle.
            CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
              EXPORTING
                node_data = url_prgn.
          ENDIF.
        ELSEIF lv_contingencia = abap_false.
          DATA: p_out  TYPE  xstring,
                p_name TYPE  string.

          CALL FUNCTION 'Z_GRC_ARQUIVO_DOC_XML'
            EXPORTING
              i_docnum = doc_numero
              i_tipo   = 'PDF'
            IMPORTING
              out      = p_out
              e_name   = p_name.

        ENDIF.
      ELSE.
        MESSAGE e038 RAISING nao_localizado.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE e000 WITH doc_numero RAISING nao_localizado.
  ENDIF.

ENDFUNCTION.
