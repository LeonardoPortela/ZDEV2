*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 16/02/2021                                              &*
*& Descrição: Controle Fatura Eletrônica com Tarnet                   &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zfiy0039.

TABLES: j_1acae.

*---------------------------------------------------------------------*
* TYPES                                                               *
*---------------------------------------------------------------------*
TYPES: BEGIN OF ty_j_1acae.
         INCLUDE STRUCTURE j_1acae.
         TYPES:  del(1),
       END OF ty_j_1acae.

TYPES: BEGIN OF ty_zfity0035,
         bukrs       TYPE zfiyt0035-bukrs,
         brnch       TYPE zfiyt0035-brnch,
         cae_ref     TYPE zfiyt0035-cae_ref,
         cae_refyr   TYPE zfiyt0035-cae_refyr,
         budat       TYPE zfiyt0035-budat,
         xblnr       TYPE zfiyt0035-xblnr,
         kunnr       TYPE zfiyt0035-kunnr,
         cae_num     TYPE zfiyt0035-cae_num,
         cae_duedate TYPE zfiyt0035-cae_duedate,
         doccls      TYPE zfiyt0035-doccls,
         j_1aprtchr  TYPE zfiyt0035-j_1aprtchr,
       END OF ty_zfity0035.


TYPES: BEGIN OF ty_desconto_mi,
         vbeln TYPE  vbrk-vbeln,
         knumv TYPE  konv-knumv,
         kschl TYPE  konv-kschl,
         vtext TYPE  t685t-vtext,
         kwert TYPE  string,
       END OF ty_desconto_mi.


TYPES: BEGIN OF ty_desconto_exp,
         vbeln TYPE  vbrk-vbeln,
         knumv TYPE  konv-knumv,
         kschl TYPE  konv-kschl,
         vtext TYPE  t685t-vtext,
         kwert TYPE  string,
       END OF ty_desconto_exp.

TYPES: BEGIN OF ty_saida_mi,
         bukrs_key     TYPE j_1acae-bukrs,
         brnch_key     TYPE j_1acae-brnch,
         cae_ref_key   TYPE j_1acae-cae_ref,
         cae_refyr_key TYPE j_1acae-cae_refyr,
         budat_key     TYPE j_1acae-budat,
         xblnr_key     TYPE j_1acae-xblnr,
**** Esse campos abaixo são os que vão na saida do JSON
         cae_ref       TYPE j_1acae-cae_ref,
         brnch         TYPE j_1acae-brnch,
         xblnr         TYPE j_1acae-xblnr,
         budat         TYPE string,
         name1         TYPE kna1-name1,
         stras         TYPE kna1-stras,
         pstlz         TYPE kna1-pstlz,
         ort01         TYPE kna1-ort01,
         stcd1         TYPE kna1-stcd1,
         clidom(90)    TYPE c,
         bstkd         TYPE vbkd-bstkd,
         inco2         TYPE vbkd-inco2,
         lifnr         TYPE zsdyt0049-lifnr,
         name1_lfa     TYPE lfa1-name1,
         stras_lfa     TYPE lfa1-stras,
         stcd1_lfa     TYPE lfa1-stcd1,
         matnr         TYPE string,
         arktx         TYPE vbrp-arktx,
         fkimg         TYPE string,
         netwr         TYPE string,
         kwert         TYPE konv-kwert,
         xtotal        TYPE string,
         xpreco        TYPE string,
         total         TYPE string,
         totaltexto    TYPE string,
         tpcomp        TYPE string,
         cae_num       TYPE j_1acae-cae_num,
         cae_duedate   TYPE string,
         observacao    TYPE string,
         xdescr        TYPE string,
         deduccion(1)  TYPE c,
         xdif          TYPE string,
       END OF ty_saida_mi.


TYPES: BEGIN OF ty_saida_exp,
         bukrs_key     TYPE j_1acae-bukrs,
         brnch_key     TYPE j_1acae-brnch,
         cae_ref_key   TYPE j_1acae-cae_ref,
         cae_refyr_key TYPE j_1acae-cae_refyr,
         budat_key     TYPE j_1acae-budat,
         xblnr_key     TYPE j_1acae-xblnr,
**** Esse campos abaixo são os que vão na saida do JSON
         cae_ref       TYPE j_1acae-cae_ref,
         brnch         TYPE j_1acae-brnch,
         xblnr         TYPE j_1acae-xblnr,
         budat         TYPE string,
         name1         TYPE kna1-name1,
         stras         TYPE kna1-stras,
         pstlz         TYPE kna1-pstlz,
         ort01         TYPE kna1-ort01,
         stcd1         TYPE kna1-stcd1,
         clidom(90)    TYPE c,
         bstkd         TYPE vbkd-bstkd,
         bstdk         TYPE string,
         bstkd_e       TYPE vbkd-bstkd_e,
         inco2         TYPE vbkd-inco2,
         xcto          TYPE vbfa-vbelv, "Contrato
         landx         TYPE t005t-landx,
         lgobe         TYPE t001l-lgobe,
         fkdat         TYPE string,
         matnr         TYPE string,
         arktx         TYPE vbrp-arktx,
         fkimg         TYPE string,
         netwr         TYPE string,
         xpreco        TYPE string,
         xtotal        TYPE string,
         total         TYPE string,
         totaltexto    TYPE string,
         totalcotacao  TYPE string,
         kursf         TYPE string,
         tpcomp        TYPE string,
         cae_num       TYPE j_1acae-cae_num,
         cae_duedate   TYPE string,
         navio         TYPE string,
       END OF ty_saida_exp.

*---------------------------------------------------------------------*
* DATA                                                                *
*---------------------------------------------------------------------*

DATA: git_j_1acae       TYPE TABLE OF ty_j_1acae,
      git_zfiyt0035     TYPE TABLE OF zfiyt0035,
      git_zfiyt0035_aux TYPE TABLE OF zfiyt0035,
      git_zfiyt0035_exp TYPE TABLE OF ty_zfity0035,
      git_zfiyt0035_mi  TYPE TABLE OF ty_zfity0035,
      git_zsdyt0049     TYPE  STANDARD TABLE OF zsdyt0049,
      git_lfa1          TYPE STANDARD TABLE OF lfa1,
      git_kna1          TYPE STANDARD TABLE OF kna1,
      git_vbrp          TYPE STANDARD TABLE OF vbrp,
      git_vbrk          TYPE STANDARD TABLE OF vbrk,
      git_konv          TYPE STANDARD TABLE OF konv,
      git_t685t         TYPE STANDARD TABLE OF t685t,
      git_t005t         TYPE STANDARD TABLE OF t005t,
      git_vbfa          TYPE STANDARD TABLE OF vbfa,
      git_vbfa_cont     TYPE STANDARD TABLE OF vbfa,
      git_vbkd          TYPE STANDARD TABLE OF vbkd,
      git_vbap          TYPE STANDARD TABLE OF vbap,
      git_t001l         TYPE STANDARD TABLE OF t001l,
      git_desconto_mi   TYPE TABLE OF ty_desconto_mi,
      git_desconto_exp  TYPE TABLE OF ty_desconto_exp,
      git_saida_exp     TYPE TABLE OF ty_saida_exp,
      git_saida_mi      TYPE TABLE OF ty_saida_mi.


DATA: gwa_j_1acae       TYPE ty_j_1acae,
      gwa_zfiyt0035_exp TYPE ty_zfity0035,
      gwa_zfiyt0035_mi  TYPE ty_zfity0035,
      gwa_zfiyt0035     TYPE zfiyt0035,
      gwa_zsdyt0049     TYPE zsdyt0049,
      gwa_lfa1          TYPE lfa1,
      gwa_kna1          TYPE kna1,
      gwa_vbrp          TYPE vbrp,
      gwa_vbrk          TYPE vbrk,
      gwa_konv          TYPE konv,
      gwa_t685t         TYPE t685t,
      gwa_t005t         TYPE t005t,
      gwa_vbfa          TYPE vbfa,
      gwa_vbfa_cont     TYPE vbfa,
      gwa_vbkd          TYPE vbkd,
      gwa_vbap          TYPE vbap,
      gwa_t001l         TYPE t001l,
      gwa_desconto_mi   TYPE ty_desconto_mi,
      gwa_desconto_exp  TYPE ty_desconto_exp,
      gwa_saida_exp     TYPE ty_saida_exp,
      gwa_saida_mi      TYPE ty_saida_mi.


DATA: gva_url           TYPE string,
      gva_url_mi        TYPE string,
      gva_url_exp       TYPE string,
      gva_url_pdf       TYPE string,
      gva_url_token     TYPE string,
      gva_url_token_mi  TYPE string,
      gva_url_token_exp TYPE string,
      gva_url_token_pdf TYPE string,
      gva_usuario       TYPE zde_ui_src_username60,
      gva_senha         TYPE zde_ui_src_password60,
      gva_token         TYPE string,
      gva_json_input    TYPE string,
      gva_json_retorno  TYPE string,
      gva_tabix         TYPE sy-tabix,
      gva_count         TYPE i,
      gva_filename      TYPE zfiyt0035-file_name,
      gva_fkimg_t       TYPE p DECIMALS 3,
      gva_fkimg_c(30)   TYPE c.


DATA: gva_len              TYPE i,
      gva_xblnr            TYPE j_1acae-xblnr,
      gva_fkimg            TYPE string,
      gva_xpreco           TYPE konv-kbetr,
      gva_xtotal           TYPE konv-kwert,
      gva_amount           TYPE konv-kwert,
      gva_kmein            TYPE konv-kmein,
      gva_in_words         TYPE spell,
      gva_con              TYPE string,
      gva_centavos         TYPE string,
      gva_texto_moneda     TYPE string,
      gva_text_cotizacion  TYPE string,
      gva_cotizacion       TYPE string,
      gva_text_cotizacion2 TYPE string,
      gva_dtime            TYPE string,
      gva_dtime_t          TYPE timestamp,
      gva_tpcomp           TYPE string,
      gva_xdescr           TYPE string.



*---------------------------------------------------------------------*
* RANGES                                                              *
*---------------------------------------------------------------------*
RANGES: gra_budat FOR j_1acae-budat.

*---------------------------------------------------------------------*
* TAG                                                                 *
*---------------------------------------------------------------------*
DEFINE add_tag.
  CONCATENATE gva_json_input '' &1 ': ' &2 '' &3 INTO gva_json_input.
END-OF-DEFINITION.

DEFINE add_tag_chave.
  CONCATENATE gva_json_input '' &1 '' &2 '' &3 INTO gva_json_input.
END-OF-DEFINITION.
DEFINE add_tag_string.
  CONCATENATE gva_json_input '' &1 ': '  '"' &2 '"' '' &3 INTO gva_json_input.
END-OF-DEFINITION.
*---------------------------------------------------------------------*
* CLASS                                                               *
*---------------------------------------------------------------------*
DATA(cl_fiy_qrcode) = NEW zcl_fiy_qrcode( ).

*--------------------------------------------------------------------*
* OPCIONES
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_budat  FOR j_1acae-budat.
SELECTION-SCREEN END OF BLOCK b1.

PERFORM f_seleciona_dados.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  IF sy-batch = 'X'.
    DATA: new_date TYPE sy-datum.
    new_date = sy-datum - 30.

    CLEAR: gra_budat.
    gra_budat-sign   = 'I'.
    gra_budat-option = 'BT'.
    gra_budat-low    = new_date.
    gra_budat-high   = sy-datum.
    APPEND gra_budat.

    SELECT * INTO TABLE git_j_1acae
      FROM j_1acae
     WHERE bukrs      EQ '0100'
       AND budat      IN gra_budat
       AND cae_status EQ 'A'.

  ELSE.

    SELECT * INTO TABLE git_j_1acae
      FROM j_1acae
     WHERE bukrs      EQ '0100'
       AND budat      IN p_budat
       AND cae_status EQ 'A'.

  ENDIF.

  IF git_j_1acae IS NOT INITIAL.

    SELECT bukrs
           brnch
           cae_ref
           cae_refyr
           budat
           xblnr
           kunnr
           cae_num
           cae_duedate
           doccls
           j_1aprtchr
       INTO CORRESPONDING FIELDS OF TABLE git_zfiyt0035
    FROM zfiyt0035
    FOR ALL ENTRIES IN git_j_1acae
    WHERE  bukrs      EQ git_j_1acae-bukrs
       AND brnch      EQ git_j_1acae-brnch
       AND cae_ref    EQ git_j_1acae-cae_ref
       AND cae_refyr  EQ git_j_1acae-cae_refyr
       AND budat      EQ git_j_1acae-budat.


    IF git_zfiyt0035 IS NOT INITIAL.

      LOOP AT git_j_1acae INTO gwa_j_1acae.

        gva_tabix = sy-tabix.
        READ TABLE git_zfiyt0035 INTO gwa_zfiyt0035 WITH KEY bukrs  = gwa_j_1acae-bukrs
                                          brnch     = gwa_j_1acae-brnch
                                          cae_ref   = gwa_j_1acae-cae_ref
                                          cae_refyr = gwa_j_1acae-cae_refyr
                                          budat     = gwa_j_1acae-budat.
        IF sy-subrc EQ 0.
          gwa_j_1acae-del = 'X'.
        ENDIF.
        MODIFY git_j_1acae FROM gwa_j_1acae INDEX gva_tabix  TRANSPORTING del.
        CLEAR: gwa_j_1acae.
      ENDLOOP.
    ENDIF.

    DELETE git_j_1acae WHERE del = 'X'.

    IF git_j_1acae IS NOT INITIAL.

      CLEAR: git_zfiyt0035[].

      LOOP AT git_j_1acae INTO gwa_j_1acae.

        gwa_zfiyt0035-bukrs        = gwa_j_1acae-bukrs.
        gwa_zfiyt0035-brnch        = gwa_j_1acae-brnch.
        gwa_zfiyt0035-cae_ref      = gwa_j_1acae-cae_ref.
        gwa_zfiyt0035-cae_refyr    = gwa_j_1acae-cae_refyr.
        gwa_zfiyt0035-budat        = gwa_j_1acae-budat.
        gwa_zfiyt0035-xblnr        = gwa_j_1acae-xblnr.
        gwa_zfiyt0035-kunnr        = gwa_j_1acae-kunnr.
        gwa_zfiyt0035-cae_num      = gwa_j_1acae-cae_num.
        gwa_zfiyt0035-cae_duedate  = gwa_j_1acae-cae_duedate.
        gwa_zfiyt0035-doccls       = gwa_j_1acae-doccls .
        gwa_zfiyt0035-j_1aprtchr   = gwa_j_1acae-j_1aprtchr.

        APPEND gwa_zfiyt0035 TO git_zfiyt0035.
        CLEAR:gwa_zfiyt0035.

      ENDLOOP.

      MODIFY zfiyt0035 FROM TABLE git_zfiyt0035[].
      COMMIT WORK.

    ENDIF.

* Processo de VENDA MERCA INTERNO
    SELECT bukrs
           brnch
           cae_ref
           cae_refyr
           budat
           xblnr
           kunnr
           cae_num
           cae_duedate
           doccls
           j_1aprtchr
      INTO CORRESPONDING FIELDS OF TABLE git_zfiyt0035_mi
      FROM zfiyt0035
     WHERE status_env = ''
      AND brnch EQ '0006'.


    IF git_zfiyt0035_mi IS NOT INITIAL.

      SELECT *
      FROM zsdyt0049
      INTO TABLE git_zsdyt0049
      FOR ALL ENTRIES IN git_zfiyt0035_mi
              WHERE doc_fat	=	git_zfiyt0035_mi-cae_ref.

      IF git_zsdyt0049 IS NOT INITIAL.
        SELECT *
        FROM lfa1
        INTO TABLE git_lfa1
        FOR ALL ENTRIES IN git_zsdyt0049
                WHERE lifnr	=	git_zsdyt0049-lifnr.
      ENDIF.

      SELECT *
      FROM kna1
      INTO TABLE git_kna1
      FOR ALL ENTRIES IN git_zfiyt0035_mi
              WHERE kunnr	=	git_zfiyt0035_mi-kunnr.

      SELECT *
      FROM vbrp
      INTO TABLE git_vbrp
      FOR ALL ENTRIES IN git_zfiyt0035_mi
            WHERE vbeln	=	git_zfiyt0035_mi-cae_ref.


      SELECT *
      FROM vbfa
      INTO TABLE git_vbfa
      FOR ALL ENTRIES IN git_zfiyt0035_mi
           WHERE vbeln  = git_zfiyt0035_mi-cae_ref
              AND vbtyp_n IN ('M' , 'O').

      IF git_vbfa IS NOT INITIAL.
        SELECT *
          FROM vbkd
          INTO TABLE git_vbkd
          FOR ALL ENTRIES IN git_vbfa
               WHERE vbeln  = git_vbfa-vbelv.
      ENDIF.


      SELECT *
        FROM vbrk
        INTO TABLE git_vbrk
        FOR ALL ENTRIES IN git_zfiyt0035_mi
            WHERE vbeln	=	git_zfiyt0035_mi-cae_ref.

      IF git_vbrk IS NOT INITIAL.
        SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @GIT_VBRK WHERE KNUMV = @GIT_VBRK-KNUMV AND KSCHL IN ( 'J1AU' , 'J1X2' , 'J1X1' , 'PR00' ) INTO CORRESPONDING FIELDS OF TABLE @GIT_KONV .
      ENDIF.

      SELECT *
       INTO TABLE git_t685t
       FROM t685t
      WHERE spras  = 'S'
       AND kschl  IN ('J1AU', 'J1X2', 'J1X1').

      PERFORM f_organiza_dados_mi.

    ENDIF.

*  Processo de VENDA EXPORTAÇÃO.

    PERFORM f_refresh.

    SELECT bukrs
           brnch
           cae_ref
           cae_refyr
           budat
           xblnr
           kunnr
           cae_num
           cae_duedate
           doccls
           j_1aprtchr
     INTO CORRESPONDING FIELDS OF TABLE git_zfiyt0035_exp
      FROM zfiyt0035
     WHERE status_env = ''
      AND brnch EQ '0005'.

    IF git_zfiyt0035_exp IS NOT INITIAL.

      SELECT *
      FROM kna1
      INTO TABLE git_kna1
      FOR ALL ENTRIES IN git_zfiyt0035_exp
          WHERE kunnr	=	git_zfiyt0035_exp-kunnr.

      IF git_kna1 IS NOT INITIAL.

        SELECT *
        FROM t005t
        INTO TABLE git_t005t
        FOR ALL ENTRIES IN git_kna1
          WHERE spras	=	'S'
          AND land1 = git_kna1-land1.

      ENDIF.

      SELECT *
      FROM vbrp
      INTO TABLE git_vbrp
      FOR ALL ENTRIES IN git_zfiyt0035_exp
            WHERE vbeln	=	git_zfiyt0035_exp-cae_ref.

      SELECT *
      FROM vbrk
      INTO TABLE git_vbrk
      FOR ALL ENTRIES IN git_zfiyt0035_exp
      WHERE vbeln	=	git_zfiyt0035_exp-cae_ref.


      IF git_vbrk IS NOT INITIAL.
        SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @GIT_VBRK WHERE KNUMV = @GIT_VBRK-KNUMV AND KSCHL IN ( 'J1AU' , 'J1X2' , 'J1X1' , 'PR00' ) INTO CORRESPONDING FIELDS OF TABLE @GIT_KONV .
      ENDIF.

      SELECT *
       INTO TABLE git_t685t
       FROM t685t
      WHERE spras  = 'S'
       AND kschl  IN ('J1AU', 'J1X2', 'J1X1').

* Busca de Dados – Permiso de Embarque

      SELECT *
        FROM vbfa
        INTO TABLE git_vbfa
        FOR ALL ENTRIES IN git_zfiyt0035_exp
             WHERE vbeln  = git_zfiyt0035_exp-cae_ref
               AND vbtyp_v  = 'C'.

      IF git_vbfa IS NOT INITIAL.
        SELECT *
          FROM vbkd
          INTO TABLE git_vbkd
          FOR ALL ENTRIES IN git_vbfa
               WHERE vbeln  = git_vbfa-vbelv.

* Busca do Contrato

        SELECT *
          FROM vbfa
          INTO TABLE git_vbfa_cont
          FOR ALL ENTRIES IN git_vbfa
               WHERE vbeln    = git_vbfa-vbelv
                 AND vbtyp_v  = 'G'.

* Busca do Deposito:

        SELECT *
          FROM vbap
          INTO TABLE git_vbap
          FOR ALL ENTRIES IN git_vbfa
               WHERE vbeln  = git_vbfa-vbelv.

        IF  git_vbap IS NOT INITIAL.
          SELECT *
            FROM t001l
            INTO TABLE git_t001l
            FOR ALL ENTRIES IN git_vbap
                 WHERE werks  = git_vbap-werks
                  AND  lgort  = git_vbap-lgort.
        ENDIF.
      ENDIF.

      PERFORM f_organiza_dados_exp.

    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_organiza_dados_mi .

  DATA: lva_name  TYPE  thead-tdname.

  DATA: lva_rate_text(15) TYPE c.

  DATA: lit_lines TYPE TABLE OF tline,
        lwa_lines LIKE LINE OF lit_lines.

  LOOP AT git_zfiyt0035_mi INTO gwa_zfiyt0035_mi.
    CLEAR: lva_name, lit_lines , lwa_lines.

*** Chaves para UPDATE na tabela zfiyt0035
    gwa_saida_mi-bukrs_key      = gwa_zfiyt0035_mi-bukrs.
    gwa_saida_mi-brnch_key      = gwa_zfiyt0035_mi-brnch.
    gwa_saida_mi-cae_ref_key    = gwa_zfiyt0035_mi-cae_ref.
    gwa_saida_mi-cae_refyr_key  = gwa_zfiyt0035_mi-cae_refyr.
    gwa_saida_mi-budat_key      = gwa_zfiyt0035_mi-budat.
    gwa_saida_mi-xblnr_key      = gwa_zfiyt0035_mi-xblnr.

    gwa_saida_mi-cae_ref = gwa_zfiyt0035_mi-cae_ref.

    lva_name = gwa_zfiyt0035_mi-cae_ref.

*** Texto Observação.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = '0001'
        language                = 'S'
        name                    = lva_name
        object                  = 'VBBK'
      TABLES
        lines                   = lit_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      IF lit_lines IS NOT INITIAL.
        LOOP AT lit_lines INTO lwa_lines WHERE tdformat EQ '*'.
          IF lwa_lines-tdline IS NOT INITIAL.
            IF gwa_saida_mi-observacao IS INITIAL.
              CONCATENATE lwa_lines-tdline gwa_saida_mi-observacao
              INTO gwa_saida_mi-observacao.
            ELSE.
              CONCATENATE lwa_lines-tdline gwa_saida_mi-observacao
              INTO gwa_saida_mi-observacao SEPARATED BY space. "'0D0A'.  " Hex for newline
            ENDIF.
            CLEAR: lwa_lines.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = '0001'
          language                = 'P'
          name                    = lva_name
          object                  = 'VBBK'
        TABLES
          lines                   = lit_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc IS INITIAL.
        IF lit_lines IS NOT INITIAL.
          LOOP AT lit_lines INTO lwa_lines WHERE tdformat EQ '*'.
            IF lwa_lines-tdline IS NOT INITIAL.
              IF gwa_saida_mi-observacao IS INITIAL.
                CONCATENATE lwa_lines-tdline gwa_saida_mi-observacao
                INTO gwa_saida_mi-observacao.
              ELSE.
                CONCATENATE lwa_lines-tdline gwa_saida_mi-observacao
                INTO gwa_saida_mi-observacao SEPARATED BY space. "'0D0A'.  " Hex for newline
              ENDIF.
              CLEAR: lwa_lines.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*** Texto Observação.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_zfiyt0035_mi-brnch
      IMPORTING
        output = gwa_saida_mi-brnch.

    CLEAR: gva_xblnr.

    gva_xblnr =  gwa_zfiyt0035_mi-xblnr+6.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gva_xblnr
      IMPORTING
        output = gwa_saida_mi-xblnr.

    CLEAR:  gva_dtime,
            gva_dtime_t.

    CONCATENATE gwa_zfiyt0035_mi-budat sy-uzeit INTO gva_dtime.

    gva_dtime_t =  gva_dtime.

    CALL METHOD cl_xlf_date_time=>create(
      EXPORTING
        timestamp = gva_dtime_t
      RECEIVING
        iso8601   = gwa_saida_mi-budat ).

    READ TABLE git_kna1 INTO gwa_kna1 WITH KEY kunnr  = gwa_zfiyt0035_mi-kunnr.

    gwa_saida_mi-name1   =   gwa_kna1-name1.
    gwa_saida_mi-stras   =   gwa_kna1-stras.
    gwa_saida_mi-pstlz   =   gwa_kna1-pstlz.
    gwa_saida_mi-ort01   =   gwa_kna1-ort01.

    IF gwa_kna1-stcd1 IS INITIAL.
      gwa_saida_mi-stcd1   = '0'.
    ELSE.
      gwa_saida_mi-stcd1 = gwa_kna1-stcd1.
    ENDIF.

    CONCATENATE gwa_saida_mi-stras gwa_saida_mi-pstlz gwa_saida_mi-ort01 INTO gwa_saida_mi-clidom
      SEPARATED BY space.

    READ TABLE git_zsdyt0049 INTO gwa_zsdyt0049 WITH KEY doc_fat  = gwa_zfiyt0035_mi-cae_ref.

    IF gwa_zsdyt0049-lifnr IS NOT INITIAL.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gwa_zsdyt0049-lifnr
        IMPORTING
          output = gwa_saida_mi-lifnr.

      CONDENSE gwa_saida_mi-lifnr NO-GAPS.


      READ TABLE git_lfa1 INTO gwa_lfa1 WITH KEY lifnr  = gwa_zsdyt0049-lifnr.

      gwa_saida_mi-name1_lfa   =  gwa_lfa1-name1.
      gwa_saida_mi-stras_lfa   =  gwa_lfa1-stras.

      IF gwa_lfa1-stcd1 IS INITIAL.
        gwa_saida_mi-stcd1_lfa   = '0'.
      ELSE.
        gwa_saida_mi-stcd1_lfa = gwa_lfa1-stcd1.
      ENDIF.
    ENDIF.

    CLEAR: gva_tpcomp.
    CONCATENATE gwa_zfiyt0035_mi-doccls  gwa_zfiyt0035_mi-j_1aprtchr INTO gva_tpcomp.

    CONDENSE gva_tpcomp NO-GAPS.

    CASE gva_tpcomp.
      WHEN 'AA'.
        gwa_saida_mi-tpcomp = '1'.

        IF gwa_zsdyt0049-ztrib = 'X'.
          gwa_saida_mi-xdescr = 'MONOTRIBUTISTA'.

          IF gwa_saida_mi-observacao IS INITIAL.
            CONCATENATE 'El crédito fiscal discriminado en el presente comprobante, sólo podrá ser computado a efectos del Régimen de Sostenimiento e '
                        'Inclusión Fiscal para Pequeños Contribuyentes de la Ley Nº 27.618' gwa_saida_mi-observacao
            INTO gwa_saida_mi-observacao.
          ELSE.
            CONCATENATE gwa_saida_mi-observacao 'El crédito fiscal discriminado en el presente comprobante, sólo podrá ser computado a efectos del Régimen de Sostenimiento e '
                        'Inclusión Fiscal para Pequeños Contribuyentes de la Ley Nº 27.618'
            INTO gwa_saida_mi-observacao SEPARATED BY space. "'0D0A'.  " Hex for newline
          ENDIF.

        ENDIF.

      WHEN 'DA'.
        gwa_saida_mi-tpcomp = '2'.
      WHEN 'CA'.
        gwa_saida_mi-tpcomp = '3'.
      WHEN 'AB'.
        gwa_saida_mi-tpcomp = '6'.
      WHEN 'DB'.
        gwa_saida_mi-tpcomp = '7'.
      WHEN 'CB'.
        gwa_saida_mi-tpcomp = '8'.
      WHEN 'AC'.
        gwa_saida_mi-tpcomp = '11'.
      WHEN 'DC'.
        gwa_saida_mi-tpcomp = '12'.
      WHEN 'CC'.
        gwa_saida_mi-tpcomp = '13'.
      WHEN 'CE'.
        gwa_saida_mi-tpcomp = '21'.
    ENDCASE.

    IF gwa_zsdyt0049-ztrib <> 'X'.
      gwa_saida_mi-xdescr = 'IVA Responsable Inscripto'.
    ENDIF.



    READ TABLE git_vbfa INTO gwa_vbfa WITH  KEY vbeln  = gwa_zfiyt0035_mi-cae_ref.

    IF sy-subrc = 0.

      READ TABLE git_vbkd INTO gwa_vbkd WITH  KEY vbeln  = gwa_vbfa-vbelv.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gwa_vbkd-bstkd
        IMPORTING
          output = gwa_saida_mi-bstkd.

      gwa_saida_mi-inco2 = gwa_vbkd-inco2.

    ENDIF.

    READ TABLE git_vbrp INTO gwa_vbrp WITH  KEY vbeln  = gwa_zfiyt0035_mi-cae_ref.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_vbrp-matnr
      IMPORTING
        output = gwa_saida_mi-matnr.

    CONDENSE gwa_saida_mi-matnr NO-GAPS.


    gwa_saida_mi-arktx   =    gwa_vbrp-arktx.


    MOVE  gwa_vbrp-netwr TO gwa_saida_mi-netwr.
    CONDENSE gwa_saida_mi-netwr NO-GAPS.

    READ TABLE git_vbrk INTO gwa_vbrk WITH KEY vbeln  = gwa_zfiyt0035_mi-cae_ref.

    IF sy-subrc = 0.

      CLEAR: gva_xtotal, gva_xpreco, gva_kmein.
      LOOP AT git_konv INTO gwa_konv WHERE knumv EQ gwa_vbrk-knumv.

        "Condição de Impostos:
        IF  gwa_konv-kschl  EQ 'J1AU'  OR
            gwa_konv-kschl  EQ 'J1X2'  OR
            gwa_konv-kschl EQ  'J1X1'.

          gva_xtotal = gva_xtotal + gwa_konv-kwert.

          READ TABLE git_t685t INTO gwa_t685t WITH KEY kschl = gwa_konv-kschl.

          gwa_desconto_mi-vbeln =   gwa_vbrk-vbeln.
          gwa_desconto_mi-knumv =   gwa_konv-knumv.
          gwa_desconto_mi-kschl =   gwa_konv-kschl.
          gwa_desconto_mi-vtext =   gwa_t685t-vtext.

          MOVE gwa_konv-kwert  TO gwa_desconto_mi-kwert.
          CONDENSE gwa_desconto_mi-kwert NO-GAPS.

          APPEND gwa_desconto_mi TO git_desconto_mi.
          CLEAR: gwa_desconto_mi, gwa_t685t.

        ELSE.
          "Condição de Preço :
          IF gwa_konv-kschl = 'PR00'.
            gva_xpreco =  gva_xpreco + gwa_konv-kbetr.
            gva_kmein =  gwa_konv-kmein.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF gva_kmein = 'UN'.

        CLEAR gva_fkimg.
        MOVE gwa_vbrp-fkimg TO gva_fkimg.
        CONDENSE gva_fkimg NO-GAPS.

        CONCATENATE  gva_fkimg 'UNIDAD' INTO gwa_saida_mi-fkimg SEPARATED BY space.
      ELSE.
        IF gva_kmein = 'TO'.

          CLEAR: gva_fkimg , gva_fkimg_t.

          gva_fkimg_t = ( gwa_vbrp-fkimg / 1000 ).

          WRITE gva_fkimg_t TO gva_fkimg_c.

          MOVE gva_fkimg_c TO gva_fkimg.
          CONDENSE gva_fkimg NO-GAPS.

          CONCATENATE  gva_fkimg 'ton, metric (1000 kg)' INTO gwa_saida_mi-fkimg SEPARATED BY space.
        ENDIF.
      ENDIF.

      MOVE gva_xtotal TO gwa_saida_mi-xtotal.
      MOVE gva_xpreco TO gwa_saida_mi-xpreco.

      CLEAR: gva_amount, gva_in_words, gva_texto_moneda, gva_con, gva_centavos.

      gva_amount =  gva_xtotal +  gwa_saida_mi-netwr.

      MOVE gva_amount TO gwa_saida_mi-total.

      CONDENSE gwa_saida_mi-xtotal NO-GAPS.
      CONDENSE gwa_saida_mi-xpreco NO-GAPS.
      CONDENSE gwa_saida_mi-total NO-GAPS.

      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = gva_amount
          currency  = gwa_vbrk-waerk
          language  = 'S'
        IMPORTING
          in_words  = gva_in_words
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      gva_texto_moneda ='SON PESOS '.

      IF gva_in_words-decword NE 'CERO'.
        gva_con = 'CON '.
        gva_centavos = 'CENTAVOS'.
      ELSE.
        gva_in_words-decword = ' '.
      ENDIF.

      CONCATENATE  gva_texto_moneda  gva_in_words-word gva_con gva_in_words-decword gva_centavos INTO
      gwa_saida_mi-totaltexto SEPARATED BY space.

    ENDIF.

    gwa_saida_mi-cae_num     = gwa_zfiyt0035_mi-cae_num.

    CLEAR:  gva_dtime,
            gva_dtime_t.

    CONCATENATE gwa_zfiyt0035_mi-cae_duedate sy-uzeit INTO gva_dtime.

    gva_dtime_t =  gva_dtime.

    CALL METHOD cl_xlf_date_time=>create(
      EXPORTING
        timestamp = gva_dtime_t
      RECEIVING
        iso8601   = gwa_saida_mi-cae_duedate ).

*** 29/09/2022 - Deduccion
    IF gwa_saida_mi-netwr <> gwa_saida_mi-xpreco AND ( gwa_vbrk-fkart = 'YV21' OR  gwa_vbrk-fkart = 'YV22' ).
      gwa_saida_mi-deduccion = 'X'.
      gwa_saida_mi-xdif = ( gwa_saida_mi-xpreco - gwa_saida_mi-netwr ) * -1.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = gwa_saida_mi-xdif.

      CONDENSE gwa_saida_mi-xdif NO-GAPS.

      gwa_saida_mi-netwr = gwa_saida_mi-xpreco .

    ENDIF.
*** 29/09/2022 - Deduccion

    APPEND gwa_saida_mi TO git_saida_mi.
    CLEAR: gwa_saida_mi, gwa_zfiyt0035_mi.

  ENDLOOP.

  PERFORM fm_set_jason_mi.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_organiza_dados_exp .

  DATA: lva_awkey TYPE zfiyt0035-cae_ref,
        lva_kursf TYPE bkpf-kursf,
        lva_gjahr TYPE bkpf-gjahr,
        lva_name  TYPE  thead-tdname.

  DATA: lit_lines TYPE TABLE OF tline,
        lwa_lines LIKE LINE OF lit_lines.

  LOOP AT git_zfiyt0035_exp INTO gwa_zfiyt0035_exp.
    CLEAR: lva_name, lit_lines , lwa_lines.

*** chaves para update na tabela zfiyt0035
    gwa_saida_exp-bukrs_key      = gwa_zfiyt0035_exp-bukrs.
    gwa_saida_exp-brnch_key      = gwa_zfiyt0035_exp-brnch.
    gwa_saida_exp-cae_ref_key    = gwa_zfiyt0035_exp-cae_ref.
    gwa_saida_exp-cae_refyr_key  = gwa_zfiyt0035_exp-cae_refyr.
    gwa_saida_exp-budat_key      = gwa_zfiyt0035_exp-budat.
    gwa_saida_exp-xblnr_key      = gwa_zfiyt0035_exp-xblnr.

    gwa_saida_exp-cae_ref = gwa_zfiyt0035_exp-cae_ref.

    lva_name = gwa_zfiyt0035_exp-cae_ref.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = '0001'
        language                = 'S'
        name                    = lva_name
        object                  = 'VBBK'
      TABLES
        lines                   = lit_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc IS INITIAL.
      IF lit_lines IS NOT INITIAL.
        LOOP AT lit_lines INTO lwa_lines WHERE tdformat EQ '*'.
          IF lwa_lines-tdline IS NOT INITIAL.
            IF gwa_saida_exp-navio IS INITIAL.
              CONCATENATE lwa_lines-tdline gwa_saida_exp-navio
              INTO gwa_saida_exp-navio.
            ELSE.
              CONCATENATE lwa_lines-tdline gwa_saida_exp-navio
              INTO gwa_saida_exp-navio SEPARATED BY space. "'0D0A'.  " Hex for newline
            ENDIF.
            CLEAR: lwa_lines.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CLEAR: gva_tpcomp.
    CONCATENATE gwa_zfiyt0035_exp-doccls  gwa_zfiyt0035_exp-j_1aprtchr INTO gva_tpcomp.

    CONDENSE gva_tpcomp NO-GAPS.

    CASE gva_tpcomp.
      WHEN 'AA'.
        gwa_saida_exp-tpcomp = '1'.
      WHEN 'DA'.
        gwa_saida_exp-tpcomp = '2'.
      WHEN 'CA'.
        gwa_saida_exp-tpcomp = '3'.
      WHEN 'AB'.
        gwa_saida_exp-tpcomp = '6'.
      WHEN 'DB'.
        gwa_saida_exp-tpcomp = '7'.
      WHEN 'CB'.
        gwa_saida_exp-tpcomp = '8'.
      WHEN 'AC'.
        gwa_saida_exp-tpcomp = '11'.
      WHEN 'DC'.
        gwa_saida_exp-tpcomp = '12'.
      WHEN 'CC'.
        gwa_saida_exp-tpcomp = '13'.
      WHEN 'CE'.
        gwa_saida_exp-tpcomp = '21'.
      WHEN 'AE'.
        gwa_saida_exp-tpcomp = '19'.

    ENDCASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_zfiyt0035_exp-brnch
      IMPORTING
        output = gwa_saida_exp-brnch.

    CLEAR:  gva_xblnr.
    gva_xblnr =  gwa_zfiyt0035_exp-xblnr+5.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gva_xblnr
      IMPORTING
        output = gwa_saida_exp-xblnr.


    CLEAR: gva_dtime,
           gva_dtime_t.

    CONCATENATE gwa_zfiyt0035_exp-budat sy-uzeit INTO gva_dtime.

    gva_dtime_t =  gva_dtime.

    CALL METHOD cl_xlf_date_time=>create(
      EXPORTING
        timestamp = gva_dtime_t
      RECEIVING
        iso8601   = gwa_saida_exp-budat ).


    READ TABLE git_kna1 INTO gwa_kna1 WITH KEY kunnr  = gwa_zfiyt0035_exp-kunnr.

    gwa_saida_exp-name1   =   gwa_kna1-name1.
    gwa_saida_exp-stras   =   gwa_kna1-stras.
    gwa_saida_exp-pstlz   =   gwa_kna1-pstlz.
    gwa_saida_exp-ort01   =   gwa_kna1-ort01.

    IF gwa_kna1-stcd1 IS INITIAL.
      gwa_saida_exp-stcd1   = '0'.
    ELSE.
      gwa_saida_exp-stcd1 = gwa_kna1-stcd1.
    ENDIF.

    CONCATENATE gwa_saida_exp-stras gwa_saida_exp-pstlz gwa_saida_exp-ort01 INTO gwa_saida_exp-clidom
      SEPARATED BY space.

    READ TABLE git_t005t INTO gwa_t005t WITH  KEY land1 = gwa_kna1-land1.

    gwa_saida_exp-landx = gwa_t005t-landx.

    READ TABLE git_vbfa INTO gwa_vbfa WITH  KEY vbeln  = gwa_zfiyt0035_exp-cae_ref.

    IF sy-subrc = 0.
      READ TABLE git_vbkd INTO gwa_vbkd WITH  KEY vbeln  = gwa_vbfa-vbelv.

      gwa_saida_exp-bstkd   =  gwa_vbkd-bstkd.

      CLEAR: gva_dtime,
             gva_dtime_t.

      CONCATENATE gwa_vbkd-bstdk sy-uzeit INTO gva_dtime.

      gva_dtime_t =  gva_dtime.

      CALL METHOD cl_xlf_date_time=>create(
        EXPORTING
          timestamp = gva_dtime_t
        RECEIVING
          iso8601   = gwa_saida_exp-bstdk ).


      gwa_saida_exp-bstkd_e =  gwa_vbkd-bstkd_e.
      gwa_saida_exp-inco2   =  gwa_vbkd-inco2.

      "Contrato
      READ TABLE git_vbfa_cont INTO gwa_vbfa_cont WITH  KEY vbeln = gwa_vbfa-vbelv.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gwa_vbfa_cont-vbelv
        IMPORTING
          output = gwa_saida_exp-xcto.

      "Deposito
      READ TABLE git_vbap INTO gwa_vbap WITH KEY vbeln  = gwa_vbfa-vbelv.
      IF sy-subrc = 0.
        READ TABLE git_t001l INTO gwa_t001l WITH KEY werks  = gwa_vbap-werks
                                                     lgort = gwa_vbap-lgort.

        gwa_saida_exp-lgobe = gwa_t001l-lgobe .

      ENDIF.
    ENDIF.

    READ TABLE git_vbrk INTO gwa_vbrk WITH KEY vbeln  = gwa_zfiyt0035_exp-cae_ref.

    READ TABLE git_vbrp INTO gwa_vbrp WITH  KEY vbeln  = gwa_zfiyt0035_exp-cae_ref.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gwa_vbrp-matnr
      IMPORTING
        output = gwa_saida_exp-matnr.

    CONDENSE gwa_saida_exp-matnr NO-GAPS.


    IF ( gwa_vbrk-waerk EQ 'USD' ).

      SELECT SINGLE maktx INTO gwa_saida_exp-arktx
      FROM makt
      WHERE matnr EQ gwa_vbrp-matnr
      AND spras EQ 'E'.

      IF  gwa_saida_exp-arktx IS INITIAL.
        gwa_saida_exp-arktx   = gwa_vbrp-arktx.
      ENDIF.

    ELSE.
      gwa_saida_exp-arktx   = gwa_vbrp-arktx.
    ENDIF.

    gwa_saida_exp-fkimg   = gwa_vbrp-fkimg.

    MOVE  gwa_vbrp-netwr TO  gwa_saida_exp-netwr.
    CONDENSE gwa_saida_exp-netwr NO-GAPS.

    CLEAR: gva_dtime,
           gva_dtime_t.

    CONCATENATE gwa_vbrk-fkdat sy-uzeit INTO gva_dtime.

    gva_dtime_t =  gva_dtime.

    CALL METHOD cl_xlf_date_time=>create(
      EXPORTING
        timestamp = gva_dtime_t
      RECEIVING
        iso8601   = gwa_saida_exp-fkdat ).

    CLEAR: gva_xtotal, gva_xpreco,gva_kmein.
    LOOP AT git_konv INTO gwa_konv WHERE knumv EQ gwa_vbrk-knumv.

      "Condição de Impostos:
      IF  gwa_konv-kschl  EQ 'J1AU'  OR
          gwa_konv-kschl  EQ 'J1X2'  OR
           gwa_konv-kschl EQ  'J1X1'.

        gva_xtotal = gva_xtotal + gwa_konv-kwert.

        READ TABLE git_t685t INTO gwa_t685t WITH KEY kschl = gwa_konv-kschl.

        gwa_desconto_exp-vbeln =  gwa_vbrk-vbeln.
        gwa_desconto_exp-knumv =  gwa_konv-knumv.
        gwa_desconto_exp-kschl =  gwa_konv-kschl.
        gwa_desconto_exp-vtext =  gwa_t685t-vtext.
        gwa_desconto_exp-kwert =  gwa_konv-kwert.

        APPEND gwa_desconto_exp TO git_desconto_exp.
        CLEAR: gwa_desconto_exp, gwa_t685t.

      ELSE.
        "Condição de Preço :
        IF gwa_konv-kschl = 'PR00'.
          gva_xpreco = gva_xpreco + gwa_konv-kbetr.
          gva_kmein = gwa_konv-kmein.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF gva_kmein = 'UN'.
      CONCATENATE  gva_fkimg 'UNIDAD' INTO gwa_saida_exp-fkimg SEPARATED BY space.
    ELSE.
      IF gva_kmein = 'TO'.

        CLEAR: gva_fkimg , gva_fkimg_t, gva_fkimg_c.

        gva_fkimg_t = ( gwa_vbrp-fkimg / 1000 ).

        WRITE gva_fkimg_t TO gva_fkimg_c.

        MOVE gva_fkimg_c TO gva_fkimg.
        CONDENSE gva_fkimg NO-GAPS.

        CONCATENATE  gva_fkimg 'ton, metric (1000 kg)' INTO gwa_saida_exp-fkimg SEPARATED BY space.
      ENDIF.
    ENDIF.

    MOVE gva_xtotal TO gwa_saida_exp-xtotal.
    MOVE gva_xpreco TO gwa_saida_exp-xpreco.

    CLEAR: gva_amount, gva_in_words, gva_texto_moneda, gva_con, gva_centavos,
    gva_text_cotizacion, gva_cotizacion, gva_text_cotizacion2.

    gva_amount =  gva_xtotal +  gwa_saida_exp-netwr.

    MOVE gva_amount TO gwa_saida_exp-total.

    CONDENSE gwa_saida_exp-xtotal NO-GAPS.
    CONDENSE gwa_saida_exp-xpreco NO-GAPS.
    CONDENSE gwa_saida_exp-total NO-GAPS.


    IF ( gwa_vbrk-waerk EQ 'USD' ).

      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = gva_amount
          currency  = gwa_vbrk-waerk
          language  = 'E'
        IMPORTING
          in_words  = gva_in_words
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      gva_texto_moneda = 'DOLLARS'.
      gva_text_cotizacion = 'COTIZACION DE LA MONEDA '.

**** Busca Taxa Câmbio:
      CLEAR: lva_awkey.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gwa_zfiyt0035_exp-cae_ref
        IMPORTING
          output = lva_awkey.

      CLEAR: lva_kursf,
             lva_gjahr,
             gva_cotizacion.

      lva_gjahr = gwa_vbrk-fkdat+0(4).

      SELECT SINGLE kursf INTO lva_kursf
        FROM  bkpf
      WHERE bukrs	=	'0100'
      AND gjahr	=	lva_gjahr
      AND awkey	=	lva_awkey.

      gwa_saida_exp-kursf = lva_kursf.
      gva_cotizacion = lva_kursf.


      REPLACE ALL OCCURRENCES OF '.' IN gwa_saida_exp-kursf WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN gva_cotizacion WITH ','.

      CONDENSE gwa_saida_exp-kursf NO-GAPS.
      CONDENSE gva_cotizacion  NO-GAPS.

      CONCATENATE 'Pesos' ' = 1 Dolar' INTO gva_text_cotizacion2 SEPARATED BY space.

      IF gva_in_words-decword NE 'ZERO'.
        gva_con = 'CON '.
        gva_centavos = 'CENTAVOS'.
      ELSE.
        gva_in_words-decword = ' '.
      ENDIF.

      CONCATENATE  gva_texto_moneda  gva_in_words-word gva_con gva_in_words-decword gva_centavos INTO
      gwa_saida_exp-totaltexto SEPARATED BY space.

      CONCATENATE  'USD US Dollar'  gva_text_cotizacion  gva_cotizacion gva_text_cotizacion2 INTO
      gwa_saida_exp-totalcotacao SEPARATED BY space.

    ELSE.

      CALL FUNCTION 'SPELL_AMOUNT'
        EXPORTING
          amount    = gva_amount
          currency  = gwa_vbrk-waerk
          language  = 'S'
        IMPORTING
          in_words  = gva_in_words
        EXCEPTIONS
          not_found = 1
          too_large = 2
          OTHERS    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      gva_texto_moneda = 'SON PESOS'.

      IF gva_in_words-decword NE 'CERO'.
        gva_con = 'CON'.
        gva_centavos = 'CENTAVOS'.
      ELSE.
        gva_in_words-decword = ' '.
      ENDIF.

      CONCATENATE  gva_texto_moneda  gva_in_words-word gva_con gva_in_words-decword gva_centavos INTO
      gwa_saida_mi-totaltexto SEPARATED BY space.

      gwa_saida_exp-totalcotacao = 'ARS Peso Argentino'.
    ENDIF.

    gwa_saida_exp-cae_num     = gwa_zfiyt0035_exp-cae_num.

    CLEAR: gva_dtime,
           gva_dtime_t.

    CONCATENATE gwa_zfiyt0035_exp-cae_duedate sy-uzeit INTO gva_dtime.

    gva_dtime_t =  gva_dtime.

    CALL METHOD cl_xlf_date_time=>create(
      EXPORTING
        timestamp = gva_dtime_t
      RECEIVING
        iso8601   = gwa_saida_exp-cae_duedate ).



    APPEND gwa_saida_exp TO git_saida_exp.
    CLEAR: gwa_saida_exp, gwa_zfiyt0035_exp.

  ENDLOOP.

  PERFORM fm_set_jason_exp.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_CONNECT
*&---------------------------------------------------------------------*
FORM fm_connect  CHANGING p_token.
  PERFORM fm_get_dados_conexao.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_DADOS_CONEXAO
*&---------------------------------------------------------------------*
FORM fm_get_dados_conexao .

  CLEAR: gva_url, gva_usuario, gva_senha, gva_url_token.

  SELECT SINGLE
        z~url
        z~url_token
        FROM zauth_webservice AS z
        INTO (gva_url_mi, gva_url_token_mi)
        WHERE
  z~service = 'FACTURACION_GASTOS_0100'.


  SELECT SINGLE
        z~url
        z~url_token
        FROM zauth_webservice AS z
        INTO (gva_url_exp, gva_url_token_exp)
        WHERE
  z~service = 'FACTURACION_EXPORTADOR_0100'.


  SELECT SINGLE
        z~url
        z~url_token
        FROM zauth_webservice AS z
        INTO (gva_url_pdf, gva_url_token_pdf)
        WHERE
  z~service = 'FACTURACION_SERVICIOS_0100'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_GET_TOKEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GVA_JSON_RETORNO  text
*      <--P_P_TOKEN  text
*----------------------------------------------------------------------*
FORM fm_get_token  USING    p_gva_json_retorno
                   CHANGING p_token.

  DATA: lit_authreqtype TYPE zqrcode_authreqtype.

  CLEAR: gva_token .

  REPLACE ALL OCCURRENCES OF REGEX cl_abap_char_utilities=>cr_lf IN p_gva_json_retorno WITH ''.

  cl_fdt_json=>json_to_data(
    EXPORTING
      iv_json = p_gva_json_retorno
    CHANGING
      ca_data = lit_authreqtype ).

  IF lit_authreqtype-token IS NOT INITIAL.
    CONCATENATE '"' lit_authreqtype-token '"' INTO p_token.
  ELSE.
    MESSAGE lit_authreqtype-error TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_refresh .

  CLEAR: git_lfa1,
         git_kna1,
         git_vbrp,
         git_vbrk,
         git_konv,
         git_t685t,
         git_t005t,
         git_vbfa,
         git_vbfa_cont,
         git_vbkd,
         git_vbap,
         git_t001l,
         git_desconto_mi,
         git_desconto_exp,
         git_saida_exp,
         git_saida_mi.

  CLEAR: gwa_j_1acae,
         gwa_zfiyt0035_exp,
         gwa_zfiyt0035_mi,
         gwa_zfiyt0035,
         gwa_zsdyt0049,
         gwa_lfa1,
         gwa_kna1,
         gwa_vbrp,
         gwa_vbrk,
         gwa_konv,
         gwa_t685t,
         gwa_t005t,
         gwa_vbfa,
         gwa_vbfa_cont,
         gwa_vbkd,
         gwa_vbap,
         gwa_t001l,
         gwa_desconto_mi,
         gwa_desconto_exp,
         gwa_saida_exp,
         gwa_saida_mi.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SET_JASON_MI
*&---------------------------------------------------------------------*
FORM fm_set_jason_mi .

  PERFORM fm_connect  CHANGING gva_token.
*----------------------------------------------------------------------*
*      Carrega os dados na variavel JSON - Mercado Interno             *
*----------------------------------------------------------------------*

  DATA: lva_xblnr          TYPE j_1acae-xblnr,
        lva_fecha_comp(10) TYPE c,
        lva_clidom(80)     TYPE c.

  LOOP AT git_saida_mi INTO gwa_saida_mi.
    CLEAR: gva_json_input.

    CONCATENATE gva_json_input '{' INTO gva_json_input.

    add_tag '"PuntodeVenta"'                                      gwa_saida_mi-brnch              ','.
    add_tag '"NumeroComprobante"'                                 gwa_saida_mi-xblnr              ','.
    add_tag_string '"FechaComprobante"'                           gwa_saida_mi-budat              ','.
    add_tag_string '"Cliente"'                                    gwa_saida_mi-name1              ','.
    add_tag_string '"ClienteDomicilio"'                           gwa_saida_mi-clidom             ','.
    add_tag_string '"ClienteCondicionIva"'                        gwa_saida_mi-xdescr             ','.
    add_tag '"ClienteCUIT"'                                       gwa_saida_mi-stcd1              ','.
    add_tag_string '"Corredor"'                                   gwa_saida_mi-name1_lfa          ','.
    add_tag_string '"CorredorDomicilio"'                          gwa_saida_mi-stras_lfa          ','.
    IF gwa_saida_mi-stcd1_lfa  IS INITIAL.
      add_tag_string '"CorredorCUIT"'                              ''                              ','.
    ELSE.
      add_tag '"CorredorCUIT"'                                      gwa_saida_mi-stcd1_lfa          ','.
    ENDIF.
    add_tag_string '"SAP"'                                        gwa_saida_mi-lifnr              ','.
    add_tag_string '"Condiciones"'                                'CAD -Cash Against Document'    ','.
    add_tag_string '"NumeroPermiso"'                              ''                              ','.
    add_tag_string '"FechaPermiso"'                               ''                              ','.
    add_tag_string '"Contrato"'                                   gwa_saida_mi-bstkd              ','.
    add_tag_string '"DestinoComprobante"'                         ''                              ','.
    add_tag_string '"DestinoMercaderia"'                          ''                              ','.
    add_tag_string '"Incoterm"'                                   gwa_saida_mi-inco2              ','.
    add_tag_string '"PuertoCarga"'                                ''                              ','.
    add_tag_string '"FechaEmbarque"'                              ''                              ','.
    add_tag '"DetalleFactura"'                                    ''                              ''.
    add_tag_chave ''                                              ''                              '['.
    add_tag_chave ''                                              ''                              '{'.
    add_tag '"CodigoProducto"'                                    gwa_saida_mi-matnr              ','.
    add_tag_string '"Concepto"'                                   gwa_saida_mi-arktx              ','.
    add_tag_string '"Unidades"'                                   gwa_saida_mi-fkimg              ','.
    add_tag '"PrecioUnitario"'                                    gwa_saida_mi-xpreco             ','.
    add_tag_string '"Codigo"'                                     ''                              ','.
    add_tag_string '"Numero"'                                     ''                              ','.
    add_tag '"PrecioTotal"'                                       gwa_saida_mi-netwr              ''.

*** 29.09.2022 - DEDUCCION - Inicio
    IF gwa_saida_mi-deduccion = 'X'.
      add_tag_chave '}'                                             ','                         '{'.
      add_tag '"CodigoProducto"'                                    ''                          ','.
      add_tag_string '"Concepto"'                                   'DEDUCCION'                 ','.
      add_tag_string '"Unidades"'                                   '0'                         ','.
      add_tag '"PrecioUnitario"'                                    gwa_saida_mi-xdif           ','.
      add_tag_string '"Codigo"'                                     ''                          ','.
      add_tag_string '"Numero"'                                     ''                          ','.
      add_tag '"PrecioTotal"'                                        gwa_saida_mi-xdif          ''.
      add_tag_chave ''                                              '}'                         ''.
      add_tag_chave ''                                              ']'                         ','.

*** 29.09.2022 - DEDUCCION - Fim
    ELSE.
      add_tag_chave ''                                              '}'                              ''.
      add_tag_chave ''                                              ']'                             ','.
    ENDIF.

    add_tag_string '"TotalTexto"'                                 gwa_saida_mi-totaltexto         ','.
    add_tag_string '"Divisa"'                                     'ARS Peso argentino'            ','.
    add_tag '"DetalleDescuentos"'                                 ''                              ''.
    add_tag_chave ''                                              ''                              '['.
    add_tag_chave ''                                              ''                              '{'.

    TYPES: tt_desc_mi TYPE TABLE OF ty_desconto_mi WITH EMPTY KEY.

    DATA(lva_count) = lines( VALUE tt_desc_mi( FOR line IN git_desconto_mi WHERE ( vbeln EQ  gwa_saida_mi-cae_ref ) ( line ) ) ).

    CLEAR: gva_count.
    LOOP AT git_desconto_mi INTO gwa_desconto_mi WHERE vbeln EQ  gwa_saida_mi-cae_ref.
      gva_count = gva_count + 1.

      add_tag_string'"Codigo"'                                   gwa_desconto_mi-kschl            ','.
      add_tag_string '"CargosDescuentos"'                        gwa_desconto_mi-vtext            ','.
      add_tag '"Monto"'                                          gwa_desconto_mi-kwert            ''.

      IF gva_count = lva_count.
        add_tag_chave ''                                              '}'                         ''.
      ELSE.
        add_tag_chave '}'                                             ','                         '{'.
      ENDIF.

    ENDLOOP.
    add_tag_chave ''                                              ']'                             ','.

    add_tag '"Total"'                                              gwa_saida_mi-total             ','.
    add_tag '"Cae"'                                                gwa_saida_mi-cae_num           ','.
    add_tag_string '"FechaVtoCae"'                                 gwa_saida_mi-cae_duedate       ','.
    add_tag '"Cotizacion"'                                         '1'                            ','.
    IF gwa_saida_mi-tpcomp IS INITIAL.
      add_tag_string '"TipoComprobante"'                            ''                             ','.
    ELSE.
      add_tag '"TipoComprobante"'                                    gwa_saida_mi-tpcomp           ','.
    ENDIF.
    add_tag_string '"Observacion"'                                  gwa_saida_mi-observacao       ''.
    CONCATENATE gva_json_input '}' INTO gva_json_input.

    PERFORM fm_send_jason_mi USING gva_json_input gwa_saida_mi.

    CLEAR: gwa_saida_mi.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SET_JASON_EXP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_set_jason_exp .

  PERFORM fm_connect  CHANGING gva_token.

  LOOP AT git_saida_exp INTO gwa_saida_exp.
    CLEAR: gva_json_input.

    CONCATENATE gva_json_input '{' INTO gva_json_input.

    add_tag '"PuntodeVenta"'                                      gwa_saida_exp-brnch        ','.
    add_tag '"NumeroComprobante"'                                 gwa_saida_exp-xblnr        ','.
    add_tag_string '"FechaComprobante"'                           gwa_saida_exp-budat        ','.
    add_tag_string '"Cliente"'                                    gwa_saida_exp-name1        ','.
    add_tag_string '"ClienteDomicilio"'                           gwa_saida_exp-clidom       ','.
    add_tag_string '"ClienteCondicionIva"'                        'Cliente del Exterior'     ','.
    add_tag '"ClienteCUIT"'                                       gwa_saida_exp-stcd1        ','.
    add_tag_string '"Corredor"'                                   ''                         ','.
    add_tag_string '"CorredorDomicilio"'                          ''                         ','.
    add_tag_string '"CorredorCondicionIva"'                       ''                         ','.
    add_tag '"CorredorCUIT"'                                      '0'                        ','.
    add_tag_string '"Sap"'                                        ''                         ','.
    add_tag_string '"Condiciones"'                                'CAD -Cash Against Document' ','.
    add_tag_string '"NumeroPermiso"'                              gwa_saida_exp-bstkd        ','.
    add_tag_string '"FechaPermiso"'                               gwa_saida_exp-bstdk        ','.
    add_tag_string '"Contrato"'                                   gwa_saida_exp-xcto         ','.
    add_tag_string '"DestinoComprobante"'                         gwa_saida_exp-landx        ','.
    add_tag_string '"DestinoMercaderia"'                          gwa_saida_exp-bstkd_e      ','.
    add_tag_string '"Incoterm"'                                   gwa_saida_exp-inco2        ','.
    add_tag_string '"PuertoCarga"'                                gwa_saida_exp-lgobe        ','.
    add_tag_string '"FechaEmbarque"'                              gwa_saida_exp-fkdat        ','.
    add_tag '"DetalleFactura"'                                    ''                         ''.
    add_tag_chave ''                                              '['                        ''.
    add_tag_chave ''                                              '{'                        ''.
    add_tag '"CodigoProducto"'                                    gwa_saida_exp-matnr        ','.
    add_tag_string '"Concepto"'                                   gwa_saida_exp-arktx        ','.
    add_tag_string '"Unidades"'                                   gwa_saida_exp-fkimg        ','.
    add_tag '"PrecioUnitario"'                                    gwa_saida_exp-xpreco       ','.
    add_tag_string '"Codigo"'                                     ''                         ','.
    add_tag_string '"Numero"'                                     ''                         ','.
    add_tag '"PrecioTotal"'                                       gwa_saida_exp-netwr        ''.
    add_tag_chave ''                                              '}'                        ''.
    add_tag_chave ''                                              ']'                        ','.
    add_tag_string '"TotalTexto"'                                 gwa_saida_exp-totaltexto   ','.
    add_tag_string '"Cotizacion"'                                 gwa_saida_exp-kursf        ','.
    IF gwa_saida_exp-tpcomp IS INITIAL.
      add_tag_string '"TipoComprobante"'                           ''                        ','.
    ELSE.
      add_tag '"TipoComprobante"'                                 gwa_saida_exp-tpcomp       ','.
    ENDIF.
    add_tag_string '"Divisa"'                                     gwa_saida_exp-totalcotacao ','.
    add_tag '"DetalleDescuentos"'                                 ''                         ''.
    add_tag_chave ''                                              '['                        ''.
    add_tag_chave ''                                              '{'                        ''.

    TYPES: tt_desc_exp TYPE TABLE OF ty_desconto_exp WITH EMPTY KEY.

    DATA(lva_count) = lines( VALUE tt_desc_exp( FOR line IN git_desconto_exp WHERE ( vbeln EQ  gwa_saida_exp-cae_ref ) ( line ) ) ).

    CLEAR: gva_count.
    LOOP AT git_desconto_exp INTO gwa_desconto_exp WHERE vbeln EQ  gwa_saida_exp-cae_ref.
      gva_count = gva_count + 1.

      add_tag_string '"Codigo"'                                    gwa_desconto_exp-kschl      ','.
      add_tag_string '"CargosDescuentos"'                          gwa_desconto_exp-vtext      ','.
      add_tag '"Monto"'                                            gwa_desconto_exp-kwert      ''.

      IF gva_count = lva_count.
        add_tag_chave ''                                              '}'                      ''.
      ELSE.
        add_tag_chave '}'                                              ','                     '{'.
      ENDIF.

    ENDLOOP.
    add_tag_chave ''                                              ']'                          ','.
    add_tag '"Total"'                                              gwa_saida_exp-total         ','.
    add_tag '"Cae"'                                                gwa_saida_exp-cae_num       ','.
    add_tag_string '"FechaVtoCae"'                                 gwa_saida_exp-cae_duedate   ','.
    add_tag_string '"Navio"'                                       gwa_saida_exp-navio         ''.

    CONCATENATE gva_json_input '}' INTO gva_json_input.

    PERFORM fm_send_jason_exp USING gva_json_input gwa_saida_exp.

    CLEAR: gwa_saida_exp.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SEND_JASON_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_send_jason_mi  USING  p_json_input
                              p_gwa_saida_mi TYPE any.
*
  CLEAR: gva_json_retorno.

  cl_fiy_qrcode->set_json_to_opus(
  EXPORTING
    i_url       =  gva_url_mi
    i_url_token =  gva_url_token_mi
    i_json      =  p_json_input
  RECEIVING
    r_retorno = gva_json_retorno  ).

  IF gva_json_retorno IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF REGEX cl_abap_char_utilities=>cr_lf IN gva_json_retorno WITH ''.

    DATA: it_resulttype TYPE zfiy0039_qrresult.

    cl_fdt_json=>json_to_data(
      EXPORTING
        iv_json = gva_json_retorno
      CHANGING
        ca_data = it_resulttype ).

    IF it_resulttype-statuscode = '200'.

      CLEAR:gva_filename.

      CONCATENATE 'facturacion' gwa_saida_mi-budat_key+6(2)'-' gwa_saida_mi-budat_key+4(2)'-' gwa_saida_mi-budat_key+0(4)
      gwa_saida_mi-xblnr '.pdf' INTO gva_filename.
      CONDENSE gva_filename NO-GAPS.

      UPDATE zfiyt0035 SET
           pdf_factura   = it_resulttype-file
           dt_retorno = sy-datum
           hr_retorno = sy-uzeit
           status_env = 'E'
           file_name  =  gva_filename
           log_erro = ''
       WHERE bukrs     = gwa_saida_mi-bukrs_key
         AND brnch     = gwa_saida_mi-brnch_key
         AND cae_ref   = gwa_saida_mi-cae_ref_key
         AND cae_refyr = gwa_saida_mi-cae_refyr_key
         AND budat     = gwa_saida_mi-budat_key
         AND xblnr     = gwa_saida_mi-xblnr_key.

      COMMIT WORK.

    ELSE.
      UPDATE zfiyt0035 SET
       dt_envio = sy-datum
       hr_envio = sy-uzeit
       log_erro = it_resulttype-message
     WHERE bukrs     = gwa_saida_mi-bukrs_key
       AND brnch     = gwa_saida_mi-brnch_key
       AND cae_ref   = gwa_saida_mi-cae_ref_key
       AND cae_refyr = gwa_saida_mi-cae_refyr_key
       AND budat     = gwa_saida_mi-budat_key
       AND xblnr     = gwa_saida_mi-xblnr_key.

      COMMIT WORK.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SEND_JASON_EXP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fm_send_jason_exp  USING p_json_input
                              p_gwa_saida_exp TYPE any.

  CLEAR: gva_json_retorno.

  cl_fiy_qrcode->set_json_to_opus(
  EXPORTING
    i_url       =  gva_url_exp
    i_url_token =  gva_url_token_exp
    i_json      =  p_json_input
  RECEIVING
    r_retorno = gva_json_retorno  ).

  IF gva_json_retorno IS NOT INITIAL.

    REPLACE ALL OCCURRENCES OF REGEX cl_abap_char_utilities=>cr_lf IN gva_json_retorno WITH ''.

    DATA: it_resulttype TYPE zfiy0039_qrresult.

    cl_fdt_json=>json_to_data(
      EXPORTING
        iv_json = gva_json_retorno
      CHANGING
        ca_data = it_resulttype ).

    IF it_resulttype-statuscode = '200'.

      CLEAR:gva_filename.

      CONCATENATE 'facturacion' gwa_saida_exp-budat_key+6(2)'-' gwa_saida_exp-budat_key+4(2)'-' gwa_saida_exp-budat_key+0(4)
      gwa_saida_exp-xblnr '.pdf' INTO gva_filename.

      CONDENSE gva_filename NO-GAPS.

      UPDATE zfiyt0035 SET
         pdf_factura   = it_resulttype-file
         dt_retorno = sy-datum
         hr_retorno = sy-uzeit
         status_env = 'E'
         file_name  =  gva_filename
         log_erro = ''
     WHERE bukrs     = gwa_saida_exp-bukrs_key
       AND brnch     = gwa_saida_exp-brnch_key
       AND cae_ref   = gwa_saida_exp-cae_ref_key
       AND cae_refyr = gwa_saida_exp-cae_refyr_key
       AND budat     = gwa_saida_exp-budat_key
       AND xblnr     = gwa_saida_exp-xblnr_key.

      COMMIT WORK.

    ELSE.

      UPDATE zfiyt0035 SET
           dt_envio = sy-datum
           hr_envio = sy-uzeit
           log_erro = it_resulttype-message
       WHERE bukrs     = gwa_saida_exp-bukrs_key
         AND brnch     = gwa_saida_exp-brnch_key
         AND cae_ref   = gwa_saida_exp-cae_ref_key
         AND cae_refyr = gwa_saida_exp-cae_refyr_key
         AND budat     = gwa_saida_exp-budat_key
         AND xblnr     = gwa_saida_exp-xblnr_key.

      COMMIT WORK.

    ENDIF.
  ENDIF.

ENDFORM.
