*&---------------------------------------------------------------------*
*& Report  ZFIS39
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfis39.

*&---------------------------------------------------------------------*

TABLES: zfit0141, zfit0142, zib_nfe_forn, lfa1, zib_nfe, j_1bnfdoc.

TYPE-POOLS: vrm.

TYPES: BEGIN OF ty_doc,
         chave_nfe TYPE zib_nfe_forn-nu_chave,
         numero    TYPE zib_nfe_dist_ter-numero,
         numr_cte  TYPE zib_cte_dist_ter-numr_cte,
         docnum    TYPE zib_nfe-docnum,
         tipo      TYPE char3,
         check_cte TYPE char1,
       END OF ty_doc.

DATA: t_active           TYPE TABLE OF j_1bnfe_active,
      wa_active          TYPE j_1bnfe_active,
      t_zib_nfe          TYPE TABLE OF j_1bnfdoc,
      t_zib_nfe_dist_ter TYPE TABLE OF zib_nfe_dist_ter,
      t_zib_cte_dist_ter TYPE TABLE OF zib_cte_dist_ter,
      t_doc              TYPE TABLE OF ty_doc.


"Tabela Interna Global
DATA: git_t001 TYPE TABLE OF t001.
DATA: git_sflight TYPE TABLE OF sflight.
DATA: git_filtro TYPE zif_screen_linha_filtro_t,
      t_zfie0001 TYPE TABLE OF zfie0001.

DATA: r_tpmov  TYPE RANGE OF j_1bdirect,
      r_docnum TYPE RANGE OF j_1bdocnum,
      r_doc    TYPE RANGE OF j_1bdocnum,
      r_form   TYPE RANGE OF j_1bform.
DATA: dtdoc      TYPE sy-datum VALUE '20200301',
      lv_candnul TYPE sy-datum VALUE '00000000'.

DATA: wa_mensagem   TYPE char30,
      it_msg_return TYPE TABLE OF bapiret2,
      wa_msg_return TYPE bapiret2,
      itab          TYPE TABLE OF trtab WITH HEADER LINE,
      msg_lote      TYPE char80.


"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,
      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      gt_exc_button               TYPE ui_functions,
      wa_stable                   TYPE lvc_s_stbl,
      wa_layout                   TYPE lvc_s_layo,
      obj_custom_0110             TYPE REF TO cl_gui_custom_container,
      obj_alv_0110                TYPE REF TO cl_gui_alv_grid,
      it_fcat                     TYPE TABLE OF lvc_s_fcat.




SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(10) text-005.
PARAMETERS: r_r2 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND abc. "Terceiro

SELECTION-SCREEN COMMENT 25(10) text-004.
PARAMETERS: r_r1 RADIOBUTTON GROUP 1. "Aproprio
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs  FOR  j_1bnfdoc-bukrs OBLIGATORY,
                p_branch FOR  j_1bnfdoc-branch,
                p_docnum FOR zib_nfe-docnum MODIF ID a,
                "P_URL    FOR ZIB_NFE-DS_URL_DANFE NO-DISPLAY,
                p_chaves FOR zib_nfe_forn-nu_chave NO INTERVALS
                 MODIF ID b,
                p_docdat FOR  j_1bnfdoc-docdat NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-014.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 2 .
PARAMETER: c_nfe AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(12) text-012 FOR FIELD c_nfe.

SELECTION-SCREEN POSITION 20.
PARAMETER: c_cte AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 25(14) text-013 FOR FIELD c_cte.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b5.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 2 .
PARAMETER: c_pdf AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(12) text-010 FOR FIELD c_pdf.

SELECTION-SCREEN POSITION 20.
PARAMETER: c_xml AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 25(14) text-011 FOR FIELD c_xml.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.

"Tipo de movimento.
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-006.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 1 .
PARAMETER: c_sai AS CHECKBOX DEFAULT 'X' USER-COMMAND b MODIF ID i. "Saida
SELECTION-SCREEN COMMENT 3(07) text-007 FOR FIELD c_sai.

SELECTION-SCREEN POSITION 20.
PARAMETER: c_ent AS CHECKBOX DEFAULT 'X' USER-COMMAND b. "Entrada
SELECTION-SCREEN COMMENT 22(15) text-008 FOR FIELD c_ent.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b4.


AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_mod_screen.




START-OF-SELECTION.
  PERFORM fm_start_of_selection.
  IF t_zfie0001 IS NOT INITIAL.
    PERFORM fm_end_of_selection.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
*  PERFORM fm_dados_processa.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .
  DATA: r_arquivo TYPE xstring,
        name_file TYPE string,
        f_path    TYPE string,
        s_data    TYPE w3mime,
        l_data    TYPE TABLE OF w3mime.

  DATA: i_docnum TYPE j_1bdocnum,
        i_chave  TYPE string,
        e_name   TYPE string,
        e_tipo   TYPE string.

  DATA: lt_contents    TYPE TABLE OF sdokcntbin,
        ls_contents    TYPE sdokcntbin,
        lv_file_length TYPE i,
        lv_flag        TYPE c,
        lv_off         TYPE i,
        lv_len         TYPE i.

  DATA: lt_tab         TYPE TABLE OF string,
        lv_codepage(4) TYPE n VALUE '1160',
        lv_html        TYPE string,
        lv_receipt     TYPE xstring.


  "Verifica data se é menor que ano 2020.
  IF p_docdat-low < dtdoc.
    p_docdat-low = dtdoc.
  ENDIF.

  CLEAR: wa_msg_return.
  FREE: it_msg_return.

  "Check tipo de movimento.
  FREE: r_tpmov.
  IF r_r1 IS NOT INITIAL.
    IF c_sai IS NOT INITIAL. APPEND VALUE #( sign = 'I' option = 'EQ' low = '2' ) TO r_tpmov.  ENDIF. "Saida
  ENDIF.
  IF c_ent IS NOT INITIAL. APPEND VALUE #( sign = 'I' option = 'EQ' low = '1' ) TO r_tpmov.  ENDIF. "Entrada


  "Tipo.
  FREE: r_form.
  IF c_nfe IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NF55' ) TO r_form. "Nfe NF55
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NF56' ) TO r_form. "Nfe NF56
  ENDIF. "Nfe NF56

  IF c_cte IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NF57' ) TO r_form.
  ENDIF. "CTe NF57

  "Nota propria.
  IF r_r1 IS NOT INITIAL.
    FREE: r_doc, t_zib_nfe.
    IF p_chaves IS NOT INITIAL.
      LOOP AT p_chaves ASSIGNING FIELD-SYMBOL(<w_chave>).
        wa_active-regio     = <w_chave>-low(2).
        wa_active-nfyear    = <w_chave>-low+2(2).
        wa_active-nfmonth   = <w_chave>-low+4(2).
        wa_active-stcd1     = <w_chave>-low+6(14).
        wa_active-model     = <w_chave>-low+20(2).
        wa_active-serie     = <w_chave>-low+22(3).
        wa_active-nfnum9    = <w_chave>-low+25(9).
        wa_active-docnum9   = <w_chave>-low+34(9).
        wa_active-cdv       = <w_chave>-low+43(1).

        SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
          FROM j_1bnfe_active AS a
         WHERE regio    EQ @wa_active-regio
           AND nfyear   EQ @wa_active-nfyear
           AND nfmonth  EQ @wa_active-nfmonth
           AND stcd1    EQ @wa_active-stcd1
           AND model    EQ @wa_active-model
           AND serie    EQ @wa_active-serie
           AND nfnum9   EQ @wa_active-nfnum9
           AND docnum9  EQ @wa_active-docnum9
           AND cdv      EQ @wa_active-cdv
           AND cancel   NE @abap_true
           AND form     NE @space
           AND NOT EXISTS ( SELECT * FROM j_1bnfdoc AS d WHERE d~docnum EQ a~docnum AND d~cancel EQ @abap_true ).

        IF wa_j_1bnfe_active IS NOT INITIAL.
*        r_docnum = VALUE #( FOR l IN t_active ( sign = 'I' option = 'EQ' low = l-docnum ) ).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_j_1bnfe_active-docnum ) TO r_doc.
        ENDIF.
      ENDLOOP.


      IF r_doc IS NOT INITIAL.
        APPEND LINES OF r_doc TO p_docnum.
        FREE: t_zib_nfe.
        SELECT *
          FROM j_1bnfdoc INTO TABLE t_zib_nfe
         WHERE bukrs  IN p_bukrs
           AND docdat IN p_docdat
           AND docnum IN p_docnum
           AND branch IN p_branch
           AND form   NE space
           AND direct IN r_tpmov
           AND form   IN r_form
           AND candat EQ lv_candnul
           AND cancel EQ space.
      ENDIF.

    ELSE.
      FREE: t_zib_nfe.
      SELECT *
        FROM j_1bnfdoc INTO TABLE t_zib_nfe
       WHERE bukrs  IN p_bukrs
         AND docdat IN p_docdat
         AND docnum IN p_docnum
         AND branch IN p_branch
         AND form   NE space
         AND direct IN r_tpmov
         AND form   IN r_form
         AND candat EQ lv_candnul
         AND cancel EQ space.

    ENDIF.


    IF t_zib_nfe IS NOT INITIAL.
      SELECT * FROM j_1bnfe_active INTO TABLE t_active
        FOR ALL ENTRIES IN t_zib_nfe
        WHERE docnum EQ t_zib_nfe-docnum
        AND docsta EQ '1'
        AND scssta NE '2'.

      IF  t_active IS NOT INITIAL.

        r_docnum = VALUE #( FOR l IN t_active ( sign = 'I' option = 'EQ' low = l-docnum ) ).
        DELETE t_zib_nfe WHERE docnum  NOT IN r_docnum.

        CLEAR: f_path.

        LOOP AT t_zib_nfe INTO DATA(w_nfe).
          i_docnum = CONV #( w_nfe-docnum ).

          IF c_pdf IS NOT INITIAL.

            CLEAR: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
            FREE: lt_contents, lt_tab.

            TRY .
                CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
                  EXPORTING
                    i_docnum = i_docnum
*                   i_chave  = w_nfe-
                    i_tipo   = 'PDF'
                  IMPORTING
                    out      = r_arquivo
                    e_name   = e_name.

              CATCH zcx_doc_eletronico INTO DATA(zcx_doc_eletronico).
*                MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                    NUMBER zcx_doc_eletronico->msgno
*                      WITH zcx_doc_eletronico->msgv1
*                           zcx_doc_eletronico->msgv2
*                           zcx_doc_eletronico->msgv3
*                           zcx_doc_eletronico->msgv4 INTO DATA(lva_msg_erro).

                CLEAR: wa_msg_return.
                wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                wa_msg_return-message = w_nfe-docnum.
                wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                APPEND wa_msg_return TO it_msg_return.

                APPEND VALUE #(    mandt  = sy-mandt
                                  docnum = w_nfe-docnum
                                     chave_nfe  = ' '
                                          log   = wa_msg_return-message_v1
                                          icon  = icon_red_light ) TO t_zfie0001.

            ENDTRY.


            IF r_arquivo IS NOT INITIAL.
*** CS47168 - Inicio - CBRAND
              IF f_path IS INITIAL.
                CALL METHOD cl_gui_frontend_services=>directory_browse
                  CHANGING
                    selected_folder      = f_path
                  EXCEPTIONS
                    cntl_error           = 1
                    error_no_gui         = 2
                    not_supported_by_gui = 3
                    OTHERS               = 4.
              ENDIF.

              " CONCATENATE 'C:\PDF_PROPRIO\'w_nfe-docnum'.PDF' INTO name_file.
              CONCATENATE  f_path '\' w_nfe-docnum'.PDF' INTO name_file.
*** CS47168 - Fim - CBRAND
              TRY.
                  lv_len = xstrlen( r_arquivo ).
                  lv_file_length = lv_len.

                  WHILE lv_flag IS INITIAL.

                    IF lv_len <= 1022.
                      ls_contents-line = r_arquivo+lv_off(lv_len).
                      lv_flag = abap_true.
                    ELSE.
                      ls_contents-line = r_arquivo+lv_off(1022).
                      lv_off = lv_off + 1022.
                      lv_len = lv_len - 1022.
                    ENDIF.

                    APPEND ls_contents TO lt_contents.
                  ENDWHILE.

                  "Executando o download o arquivo no diretorio definido.
                  CALL FUNCTION 'GUI_DOWNLOAD'
                    EXPORTING
                      filename                = name_file "Local onde sera gravado o arquivo.
                      filetype                = 'BIN' "Definição do tipo do arquivo.
                    TABLES
                      data_tab                = lt_contents "Dados Binario do arquivo.
                    EXCEPTIONS
                      file_write_error        = 1
                      no_batch                = 2
                      gui_refuse_filetransfer = 3
                      invalid_type            = 4
                      no_authority            = 5
                      unknown_error           = 6
                      header_not_allowed      = 7
                      separator_not_allowed   = 8
                      filesize_not_allowed    = 9
                      header_too_long         = 10
                      dp_error_create         = 11
                      dp_error_send           = 12
                      dp_error_write          = 13
                      unknown_dp_error        = 14
                      access_denied           = 15
                      dp_out_of_memory        = 16
                      disk_full               = 17
                      dp_timeout              = 18
                      file_not_found          = 19
                      dataprovider_exception  = 20
                      control_flush_error     = 21
                      OTHERS                  = 22.

                  IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

                    APPEND VALUE #(    mandt  = sy-mandt
                                       docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = sy-msgv1
                                        icon  = icon_red_light ) TO t_zfie0001.

                    CLEAR: wa_msg_return.
                    wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                    wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { sy-msgv1 }|.
                    APPEND wa_msg_return TO it_msg_return.

                  ELSE.
                    "Gravar log de executação.
                    APPEND VALUE #(    mandt  = sy-mandt
                                       docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = 'PDF gerado c/sucesso-' && name_file
                                        icon  = icon_green_light ) TO t_zfie0001.


                  ENDIF.

                CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*                  MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                      NUMBER zcx_doc_eletronico->msgno
*                        WITH zcx_doc_eletronico->msgv1
*                             zcx_doc_eletronico->msgv2
*                             zcx_doc_eletronico->msgv3
*                             zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

                  CLEAR: wa_msg_return.
                  wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                  wa_msg_return-message = w_nfe-docnum.
                  wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                  APPEND wa_msg_return TO it_msg_return.

                  APPEND VALUE #(    mandt  = sy-mandt
                                    docnum = w_nfe-docnum
                                       chave_nfe  = ' '
                                            log   = wa_msg_return-message_v1
                                            icon  = icon_red_light ) TO t_zfie0001.

              ENDTRY.
            ELSE.

              CLEAR: wa_msg_return.
              wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
              wa_msg_return-message = w_nfe-docnum.
              wa_msg_return-message_v1 = |Documento { w_nfe-docnum } não encontrado!|.
              APPEND wa_msg_return TO it_msg_return.

              APPEND VALUE #(    mandt  = sy-mandt
                                docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = wa_msg_return-message_v1
                                        icon  = icon_red_light ) TO t_zfie0001.

            ENDIF.
          ENDIF.

          "Download XML.
          IF c_xml IS NOT INITIAL.

            CLEAR: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
            FREE: lt_contents, lt_tab.

            TRY .
                CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
                  EXPORTING
                    i_docnum = i_docnum
*                   i_chave  = w_nfe-
                    i_tipo   = 'XML'
                  IMPORTING
                    out      = r_arquivo
                    e_name   = e_name.

              CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*                MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                    NUMBER zcx_doc_eletronico->msgno
*                      WITH zcx_doc_eletronico->msgv1
*                           zcx_doc_eletronico->msgv2
*                           zcx_doc_eletronico->msgv3
*                           zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

                CLEAR: wa_msg_return.
                wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                wa_msg_return-message = w_nfe-docnum.
                wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                APPEND wa_msg_return TO it_msg_return.

                APPEND VALUE #(    mandt  = sy-mandt
                                  docnum = w_nfe-docnum
                                     chave_nfe  = ' '
                                          log   = wa_msg_return-message_v1
                                          icon  = icon_red_light ) TO t_zfie0001.

            ENDTRY.

            IF r_arquivo IS NOT INITIAL.

*** CS47168 - Inicio - CBRAND
              IF f_path IS INITIAL.
                CALL METHOD cl_gui_frontend_services=>directory_browse
                  CHANGING
                    selected_folder      = f_path
                  EXCEPTIONS
                    cntl_error           = 1
                    error_no_gui         = 2
                    not_supported_by_gui = 3
                    OTHERS               = 4.
              ENDIF.


              CONCATENATE f_path '\'w_nfe-docnum'.XML' INTO name_file.
              "CONCATENATE 'C:\XML_PROPRIO\'w_nfe-docnum'.XML' INTO name_file.
*** CS47168 - Fim - CBRAND
              TRY.
**/convert binary receipt to string
                  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
                    EXPORTING
                      im_xstring  = r_arquivo
                      im_encoding = '1100'
                    IMPORTING
                      ex_string   = lv_html.


                  CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
                    EXPORTING
                      external_name = lv_html
                    IMPORTING
                      sap_codepage  = lv_codepage
                    EXCEPTIONS
                      not_found     = 1
                      OTHERS        = 2.

                  APPEND lv_html TO lt_tab.
*

*            l_data = VALUE #( ( line = r_arquivo  ) ).
                  "Executando o download o arquivo no diretorio definido.
                  CALL FUNCTION 'GUI_DOWNLOAD'
                    EXPORTING
                      filename                = name_file "Local onde sera gravado o arquivo.
*                     filetype                = 'BIN' "Definição do tipo do arquivo.
                    TABLES
                      data_tab                = lt_tab
                    EXCEPTIONS
                      file_write_error        = 1
                      no_batch                = 2
                      gui_refuse_filetransfer = 3
                      invalid_type            = 4
                      no_authority            = 5
                      unknown_error           = 6
                      header_not_allowed      = 7
                      separator_not_allowed   = 8
                      filesize_not_allowed    = 9
                      header_too_long         = 10
                      dp_error_create         = 11
                      dp_error_send           = 12
                      dp_error_write          = 13
                      unknown_dp_error        = 14
                      access_denied           = 15
                      dp_out_of_memory        = 16
                      disk_full               = 17
                      dp_timeout              = 18
                      file_not_found          = 19
                      dataprovider_exception  = 20
                      control_flush_error     = 21
                      OTHERS                  = 22.

                  IF sy-subrc <> 0.
                    APPEND VALUE #(    mandt  = sy-mandt
                                       docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = sy-msgv1
                                        icon  = icon_red_light ) TO t_zfie0001.

                    CLEAR: wa_msg_return.
                    wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                    wa_msg_return-message =  w_nfe-docnum.
                    wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { sy-msgv1 }|.
                    APPEND wa_msg_return TO it_msg_return.


                  ELSE.
                    "Gravar log de executação.
                    APPEND VALUE #(    mandt  = sy-mandt
                                       docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = 'XML gerado c/sucesso-' && name_file
                                        icon  = icon_green_light ) TO t_zfie0001.
                  ENDIF.

                CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*                  MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                      NUMBER zcx_doc_eletronico->msgno
*                        WITH zcx_doc_eletronico->msgv1
*                             zcx_doc_eletronico->msgv2
*                             zcx_doc_eletronico->msgv3
*                             zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

                  CLEAR: wa_msg_return.
                  wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
                  wa_msg_return-message = w_nfe-docnum.
                  wa_msg_return-message_v1 = |Documento { w_nfe-docnum } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                  APPEND wa_msg_return TO it_msg_return.

                  APPEND VALUE #(    mandt  = sy-mandt
                                    docnum = w_nfe-docnum
                                       chave_nfe  = ' '
                                            log   = wa_msg_return-message_v1
                                            icon  = icon_red_light ) TO t_zfie0001.

              ENDTRY.
            ELSE.
              CLEAR: wa_msg_return.
              wa_msg_return-message_v2 = SWITCH #( w_nfe-form
                         WHEN 'NF55' THEN 'NFE'
                         WHEN 'NF57' THEN 'CTE' ).
              wa_msg_return-message = i_docnum.
              wa_msg_return-message_v1 = |Documento { i_docnum } não encontrado!|.
              APPEND wa_msg_return TO it_msg_return.

              APPEND VALUE #(    mandt  = sy-mandt
                                docnum = w_nfe-docnum
                                   chave_nfe  = ' '
                                        log   = wa_msg_return-message_v1
                                        icon  = icon_red_light ) TO t_zfie0001.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.

        CLEAR: wa_msg_return.
        wa_msg_return-message_v1 = |'NFes não não localizada'|.
        APPEND wa_msg_return TO it_msg_return.
      ENDIF.
    ELSE.
      CLEAR: wa_msg_return.
      wa_msg_return-message_v1 = |'NFes não não localizada'|.
      APPEND wa_msg_return TO it_msg_return.
    ENDIF.


    "Nota de terceiro.
    "================================================================================================
  ELSE.

    IF c_nfe IS NOT INITIAL. "Filtro notas de Nfe.
      "Validar chaves Nfe.
      SELECT * FROM zib_nfe_dist_ter INTO TABLE @DATA(t_zib_nfe_dist_ter)
        WHERE chave_nfe  IN @p_chaves
          AND dt_emissao IN @p_docdat
          AND ( bukrs    IN @p_bukrs OR bukrs_e IN @p_bukrs )
          AND ( branch   IN @p_branch OR branch_e IN @p_branch ).

      IF t_zib_nfe_dist_ter IS NOT INITIAL.
        t_doc = VALUE #( FOR y IN t_zib_nfe_dist_ter ( chave_nfe = y-chave_nfe   numero = y-numero  tipo = 'NFE') ).
      ENDIF.

    ENDIF.
    IF c_cte IS NOT INITIAL. "Filtro notas de Cte.
      "Validar chaves CTe.
      SELECT * FROM zib_cte_dist_ter INTO TABLE t_zib_cte_dist_ter "Filtro notas de Cte.
      WHERE cd_chave_cte  IN p_chaves
        AND docnum_cte IN p_docnum
        AND dt_emissao IN p_docdat
        AND e_tomadora IN p_bukrs
        AND f_tomadora IN p_branch.

      IF t_zib_cte_dist_ter IS NOT INITIAL.
        LOOP AT t_zib_cte_dist_ter INTO DATA(w_cte).
          APPEND VALUE #( chave_nfe = w_cte-cd_chave_cte  numero = w_cte-numr_cte  check_cte = 'X' tipo = 'CTE') TO t_doc.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF t_doc IS NOT INITIAL.
      CLEAR: f_path .
      LOOP AT t_doc INTO DATA(w_doc).
        i_chave = CONV #( w_doc-chave_nfe ).

        IF c_pdf IS NOT INITIAL.

          CLEAR: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
          FREE: lt_contents, lt_tab.

          TRY .
              CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
                EXPORTING
*                 i_docnum = i_docnum
                  i_chave = i_chave
                  i_tipo  = 'PDF'
                IMPORTING
                  out     = r_arquivo
                  e_name  = e_name.

            CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*              MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                  NUMBER zcx_doc_eletronico->msgno
*                    WITH zcx_doc_eletronico->msgv1
*                         zcx_doc_eletronico->msgv2
*                         zcx_doc_eletronico->msgv3
*                         zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

              CLEAR: wa_msg_return.
              wa_msg_return-message_v2 = w_doc-tipo.
              wa_msg_return-message = i_chave.
              wa_msg_return-message_v2 = |Documento de terceiro { i_chave } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
              APPEND wa_msg_return TO it_msg_return.

              APPEND VALUE #(    mandt  = sy-mandt
                                docnum = ''
                                   chave_nfe  = i_chave
                                        log   = wa_msg_return-message_v1
                                        icon  = icon_red_light ) TO t_zfie0001.

          ENDTRY.

          IF r_arquivo IS NOT INITIAL.
*** CS47168 - Inicio - CBRAND
            IF f_path IS INITIAL.
              CALL METHOD cl_gui_frontend_services=>directory_browse
                CHANGING
                  selected_folder      = f_path
                EXCEPTIONS
                  cntl_error           = 1
                  error_no_gui         = 2
                  not_supported_by_gui = 3
                  OTHERS               = 4.
            ENDIF.
            CONCATENATE f_path '\' w_doc-tipo '-' i_chave'.PDF' INTO name_file.
            "CONCATENATE 'C:\PDF_TERC\' w_doc-tipo '-' i_chave'.PDF' INTO name_file.
*** CS47168 - Fim - CBRAND
            TRY.
                lv_len = xstrlen( r_arquivo ).
                lv_file_length = lv_len.

                WHILE lv_flag IS INITIAL.

                  IF lv_len LE 1022.
                    ls_contents-line = r_arquivo+lv_off(lv_len).
                    lv_flag = abap_true.
                  ELSE.
                    ls_contents-line = r_arquivo+lv_off(1022).
                    lv_off = lv_off + 1022.
                    lv_len = lv_len - 1022.
                  ENDIF.

                  APPEND ls_contents TO lt_contents.
                ENDWHILE.

                CLEAR: l_data.
                CALL FUNCTION 'GUI_DOWNLOAD'
                  EXPORTING
                    filename                = name_file "Local onde sera gravado o arquivo.
                    filetype                = 'BIN' "Definição do tipo do arquivo.
                  TABLES
                    data_tab                = lt_contents "Dados Binario do arquivo.
                  EXCEPTIONS
                    file_write_error        = 1
                    no_batch                = 2
                    gui_refuse_filetransfer = 3
                    invalid_type            = 4
                    no_authority            = 5
                    unknown_error           = 6
                    header_not_allowed      = 7
                    separator_not_allowed   = 8
                    filesize_not_allowed    = 9
                    header_too_long         = 10
                    dp_error_create         = 11
                    dp_error_send           = 12
                    dp_error_write          = 13
                    unknown_dp_error        = 14
                    access_denied           = 15
                    dp_out_of_memory        = 16
                    disk_full               = 17
                    dp_timeout              = 18
                    file_not_found          = 19
                    dataprovider_exception  = 20
                    control_flush_error     = 21
                    OTHERS                  = 22.


                IF sy-subrc <> 0.
                  APPEND VALUE #(    mandt  = sy-mandt
                                     docnum = ''
                                 chave_nfe  = w_doc-chave_nfe
                                      log   = sy-msgv1
                                      icon  = icon_red_light ) TO t_zfie0001.

                  CLEAR: wa_msg_return.
                  wa_msg_return-message = w_doc-chave_nfe.
                  wa_msg_return-message_v1 = |Documento de terceiro  { w_doc-chave_nfe } { sy-msgv1 }|.
                  APPEND wa_msg_return TO it_msg_return.

                ELSE.
                  "Gravar log de executação.
                  APPEND VALUE #(    mandt  = sy-mandt
                                     docnum = ''
                                 chave_nfe  = w_doc-chave_nfe
                                      log   = 'PDF gerado c/sucesso' && name_file
                                      icon  = icon_green_light ) TO t_zfie0001.


                ENDIF.

              CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*                MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                    NUMBER zcx_doc_eletronico->msgno
*                      WITH zcx_doc_eletronico->msgv1
*                           zcx_doc_eletronico->msgv2
*                           zcx_doc_eletronico->msgv3
*                           zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

                CLEAR: wa_msg_return.
                wa_msg_return-message_v2 = w_doc-tipo.
                wa_msg_return-message = w_doc-chave_nfe.
                wa_msg_return-message_v1 = |Documento de terceiro { w_doc-chave_nfe } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                APPEND wa_msg_return TO it_msg_return.

                APPEND VALUE #(    mandt  = sy-mandt
                                  docnum = ''
                                     chave_nfe  = w_doc-chave_nfe
                                          log   = wa_msg_return-message_v1
                                          icon  = icon_red_light ) TO t_zfie0001.

            ENDTRY.

          ELSE.

            CLEAR: wa_msg_return.
            wa_msg_return-message_v2 = w_doc-tipo.
            wa_msg_return-message = w_doc-chave_nfe.
            wa_msg_return-message_v1 = |Documento de terceiro { w_doc-chave_nfe } Não encontrado!|.
            APPEND wa_msg_return TO it_msg_return.

            APPEND VALUE #(    mandt  = sy-mandt
                              docnum = ''
                                 chave_nfe  = w_doc-chave_nfe
                                      log   = wa_msg_return-message_v1
                                      icon  = icon_red_light ) TO t_zfie0001.

          ENDIF.
        ENDIF.

        "Download XML.
        IF c_xml IS NOT INITIAL.

          CLEAR: lv_len, lv_off, lv_file_length, lv_flag, ls_contents, name_file, lv_html, lv_codepage, r_arquivo, e_name.
          FREE: lt_contents, lt_tab.

          TRY.
              CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
                EXPORTING
*                 i_docnum = i_docnum
                  i_chave = i_chave
                  i_tipo  = 'XML'
                IMPORTING
                  out     = r_arquivo
                  e_name  = e_name.

            CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*              MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                  NUMBER zcx_doc_eletronico->msgno
*                    WITH zcx_doc_eletronico->msgv1
*                         zcx_doc_eletronico->msgv2
*                         zcx_doc_eletronico->msgv3
*                         zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

              CLEAR: wa_msg_return.
              wa_msg_return-message_v2 = w_doc-tipo.
              wa_msg_return-message = i_chave.
              wa_msg_return-message_v1 = |Documento de terceiro { i_chave } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
              APPEND wa_msg_return TO it_msg_return.

              APPEND VALUE #(    mandt  = sy-mandt
                                docnum = ''
                                   chave_nfe  = i_chave
                                        log   = wa_msg_return-message_v1
                                        icon  = icon_red_light ) TO t_zfie0001.

          ENDTRY.

          IF r_arquivo IS NOT INITIAL.
*** CS47168 - Inicio - CBRAND
            IF f_path IS INITIAL.
              CALL METHOD cl_gui_frontend_services=>directory_browse
                CHANGING
                  selected_folder      = f_path
                EXCEPTIONS
                  cntl_error           = 1
                  error_no_gui         = 2
                  not_supported_by_gui = 3
                  OTHERS               = 4.
            ENDIF.
            " CONCATENATE 'C:\XML_TERC\' w_doc-tipo '-' i_chave'.XML' INTO name_file.
            CONCATENATE f_path '\' w_doc-tipo '-' i_chave'.XML' INTO name_file.
*** CS47168 - Fim - CBRAND
*            l_data = VALUE #( ( line = r_arquivo  ) ).

            TRY.
**/convert binary receipt to string
                CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
                  EXPORTING
                    im_xstring  = r_arquivo
                    im_encoding = '1100'
                  IMPORTING
                    ex_string   = lv_html.


                CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
                  EXPORTING
                    external_name = lv_html
                  IMPORTING
                    sap_codepage  = lv_codepage
                  EXCEPTIONS
                    not_found     = 1
                    OTHERS        = 2.

                APPEND lv_html TO lt_tab.


                "Executando o download o arquivo no diretorio definido.
                CALL FUNCTION 'GUI_DOWNLOAD'
                  EXPORTING
                    filename                = name_file "Local onde sera gravado o arquivo.
*                   filetype                = 'BIN' "Definição do tipo do arquivo.
                  TABLES
                    data_tab                = lt_tab
                  EXCEPTIONS
                    file_write_error        = 1
                    no_batch                = 2
                    gui_refuse_filetransfer = 3
                    invalid_type            = 4
                    no_authority            = 5
                    unknown_error           = 6
                    header_not_allowed      = 7
                    separator_not_allowed   = 8
                    filesize_not_allowed    = 9
                    header_too_long         = 10
                    dp_error_create         = 11
                    dp_error_send           = 12
                    dp_error_write          = 13
                    unknown_dp_error        = 14
                    access_denied           = 15
                    dp_out_of_memory        = 16
                    disk_full               = 17
                    dp_timeout              = 18
                    file_not_found          = 19
                    dataprovider_exception  = 20
                    control_flush_error     = 21
                    OTHERS                  = 22.

                IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

                  CLEAR: wa_msg_return.
                  wa_msg_return-message_v2 = w_doc-tipo.
                  wa_msg_return-message = w_doc-chave_nfe.
                  wa_msg_return-message_v1 = |Documento de terceiro { w_doc-chave_nfe } Não encontrado!|.
                  APPEND wa_msg_return TO it_msg_return.

                  APPEND VALUE #(    mandt  = sy-mandt
                                     docnum = ''
                                 chave_nfe  = w_doc-chave_nfe
                                      log   = wa_msg_return-message_v1
                                      icon  = icon_red_light ) TO t_zfie0001.

                ELSE.
                  "Gravar log de executação.
                  APPEND VALUE #(    mandt  = sy-mandt
                                     docnum = ''
                                 chave_nfe  = w_doc-chave_nfe
                                      log   = 'XML gerado c/sucesso-' && name_file
                                      icon  = icon_green_light ) TO t_zfie0001.
                ENDIF.

              CATCH zcx_doc_eletronico INTO zcx_doc_eletronico.
*                MESSAGE ID zcx_doc_eletronico->msgid TYPE 'S'
*                    NUMBER zcx_doc_eletronico->msgno
*                      WITH zcx_doc_eletronico->msgv1
*                           zcx_doc_eletronico->msgv2
*                           zcx_doc_eletronico->msgv3
*                           zcx_doc_eletronico->msgv4 INTO lva_msg_erro.

                CLEAR: wa_msg_return.
                wa_msg_return-message_v2 = w_doc-tipo.
                wa_msg_return-message = w_doc-chave_nfe.
                wa_msg_return-message_v1 = |Documento de terceiro { w_doc-chave_nfe } { zcx_doc_eletronico->msgv1 } { zcx_doc_eletronico->msgv2 }|.
                APPEND wa_msg_return TO it_msg_return.

                APPEND VALUE #(    mandt  = sy-mandt
                                  docnum = ''
                                     chave_nfe  = w_doc-chave_nfe
                                          log   = wa_msg_return-message_v1
                                          icon  = icon_red_light ) TO t_zfie0001.

            ENDTRY.
          ELSE.

            CLEAR: wa_msg_return.
            wa_msg_return-message_v2 = w_doc-tipo.
            wa_msg_return-message = w_doc-chave_nfe.
            wa_msg_return-message_v1 = |Documento de terceiro { w_doc-chave_nfe } Não encontrado!|.
            APPEND wa_msg_return TO it_msg_return.

            APPEND VALUE #(    mandt  = sy-mandt
                               docnum = ''
                           chave_nfe  = w_doc-chave_nfe
                                log   = wa_msg_return-message_v1
                                icon  = icon_red_light ) TO t_zfie0001.

          ENDIF.
        ENDIF.
        CLEAR: w_doc, wa_msg_return.
      ENDLOOP.
    ELSE.
      CLEAR: wa_msg_return.
      wa_msg_return-message_v1 = |'NFes não não localizada'|.
      APPEND wa_msg_return TO it_msg_return.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros .

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

  LOOP AT SCREEN.
    git_filtro = VALUE #(
      ( parametro = '' valor = p_chaves )
      ( parametro = '' valor = p_docnum )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100' WITH 'Padrao'.

  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'SHOW_MSG'.
      PERFORM f_status_erro.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .
  DATA: date TYPE char10.
  date = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
  PERFORM fm_cria_fieldcat.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Download de arquivos vinculado MIRO'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Usuario'                valor = sy-uname )
                                                    ( parametro = 'Data processamento' valor = date ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      CHANGING
        it_outtab                     = t_zfie0001
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

*******************************************
  DATA: lc_col_pos  TYPE lvc_colpos.
  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
  CLEAR: git_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'zfie0001'
    CHANGING
      ct_fieldcat      = git_fcat.

  LOOP AT git_fcat ASSIGNING <fs_cat>.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_STATUS_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_status_erro .


  CHECK it_msg_return IS NOT INITIAL.
  SORT it_msg_return BY message.
  DELETE ADJACENT DUPLICATES FROM it_msg_return COMPARING message.

*  msg_lote = |Log de processamento|.
*
*  itab[] = VALUE #( FOR l IN it_msg_return ( line = l-msg ) ).
*
*  CALL FUNCTION 'LAW_SHOW_POPUP_WITH_TEXT'
*    EXPORTING
*      titelbar         = msg_lote
*    TABLES
*      list_tab         = itab[]
*    EXCEPTIONS
*      action_cancelled = 1
*      OTHERS           = 2.

  CALL SCREEN 0200 STARTING AT 5 5 ENDING AT 100 23.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR 'TB0200'.

  PERFORM fm_criar_objetos_tel_0200.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'SAIR'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS_TEL_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos_tel_0200 .

  DATA: ztotal TYPE p DECIMALS 0.

  REFRESH it_fcat.

  PERFORM alv_preenche_cat USING:
   'message_v2 ' 'Tipo              ' ' '  ''  ''  ''  ' ' ' '  '' '' '' ' ',
   'message    ' 'Docnum/Nfe        ' ' '  ''  ''  ''  ' ' ' '  '' '' '' ' ',
   'message_v1 ' 'Mensagem erro     ' ' '  ''  ''  ''  ' ' ' '  '' '' '' ' '.



  IF ( obj_custom_0110 IS INITIAL ).
    CREATE OBJECT obj_custom_0110
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER_0200'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent          = obj_custom_0110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  DESCRIBE TABLE it_msg_return LINES ztotal.
  wa_layout-grid_title = |Total de Logs de processamento: { ztotal }|.
  wa_layout-cwidth_opt = 'X'.


  CALL METHOD obj_alv_0110->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = gt_exc_button
      i_save                        = 'A'
    CHANGING
      it_fieldcatalog               = it_fcat
      it_outtab                     = it_msg_return
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0110->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0110->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2907   text
*      -->P_2908   text
*      -->P_2909   text
*      -->P_2910   text
*      -->P_2911   text
*      -->P_2912   text
*      -->P_2913   text
*      -->P_2914   text
*      -->P_2915   text
*      -->P_2916   text
*      -->P_2917   text
*      -->P_2918   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat    USING: p_campo         TYPE c
                                p_desc          TYPE c
                                p_tam           TYPE c
                                p_hot           TYPE c
                                p_zero          TYPE c
                                p_sum           TYPE c
                                p_edit          TYPE c
                                p_check         TYPE c
                                p_ref_tabname   LIKE dd02d-tabname
                                p_ref_fieldname LIKE dd03d-fieldname
                                p_tabname       LIKE dd02d-tabname
                                p_no_out        TYPE c.

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.
  wl_fcat-edit      = p_edit.
  wl_fcat-checkbox  = p_check.
  wl_fcat-ref_table = p_ref_tabname.
  wl_fcat-ref_field = p_ref_fieldname.
  wl_fcat-tabname   = p_ref_tabname.
  wl_fcat-no_out    = p_no_out.
  APPEND wl_fcat TO it_fcat.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  FM_MOD_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_mod_screen .


  IF r_r2 IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'I'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.
