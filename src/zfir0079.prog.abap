*&---------------------------------------------------------------------*
*& Report  ZFIS38
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir0079.

TABLES: zhrst_efd_e1250m, lfa1.


TYPES: BEGIN OF ty_saida,
         branch     TYPE zhrst_efd_e1250m-branch,
         budat      TYPE zhrst_efd_e1250m-budat,
         bldat      TYPE zhrst_efd_e1250m-bldat,
         ind_aquis  TYPE zhrst_efd_e1250m-ind_aquis,
         belnr      TYPE zhrst_efd_e1250m-belnr,
         xblnr      TYPE zhrst_efd_e1250m-xblnr,
         cpf        TYPE zhrst_efd_e1250m-cpf,
         lifnr      TYPE lfa1-lifnr,
         cname      TYPE zhrst_efd_e1250m-cname,
         dmbtr      TYPE zhrst_efd_e1250m-dmbtr,
         vrcp       TYPE zhrst_efd_e1250m-vrcp,
         vrrat      TYPE zhrst_efd_e1250m-vrrat,
         vrsenar    TYPE zhrst_efd_e1250m-vrsenar,
         ebeln      TYPE ekbe-ebeln,
         bsart      TYPE ekko-bsart,
         matkl      TYPE ekpo-matkl,
         matnr      TYPE ekpo-matnr,
         txz01      TYPE ekpo-txz01,
         ind_acquis TYPE zhrst_efd_e1250m-ind_acquis,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_esocial,
         status(40)    TYPE c,
         event_id(20)  TYPE c,
         event_receipt TYPE t7brefd_event-event_receipt,
         branch        TYPE zhrst_efd_e1250m-branch,
         cpf           TYPE zhrst_efd_e1250m-cpf,
         cname         TYPE zhrst_efd_e1250m-cname,
         dmbtr         TYPE zhrst_efd_e1250m-dmbtr,
         vrcp          TYPE zhrst_efd_e1250m-vrcp,
         vrrat         TYPE zhrst_efd_e1250m-vrrat,
         vrsenar       TYPE zhrst_efd_e1250m-vrsenar,
         acquisit_amt  TYPE t7brefd_rurlwrkr-acquisit_amt,
         inss_amount   TYPE t7brefd_rurlwrkr-inss_amount,
         senar_amt     TYPE t7brefd_rurlwrkr-senar_amt,
         inss_rat      TYPE t7brefd_rurlwrkr-inss_rat,
         vlr_base      TYPE zhrst_efd_e1250m-dmbtr,
         vlr_inss      TYPE zhrst_efd_e1250m-dmbtr,
         vlr_senar     TYPE zhrst_efd_e1250m-dmbtr,
         vlr_rat       TYPE zhrst_efd_e1250m-dmbtr,
       END OF ty_saida_esocial.



TYPES: BEGIN OF ty_zhrst_efd_e1250m,
         bukrs      TYPE  zhrst_efd_e1250m-bukrs,
         branch     TYPE zhrst_efd_e1250m-branch,
         budat      TYPE zhrst_efd_e1250m-budat,
         bldat      TYPE zhrst_efd_e1250m-bldat,
         ind_aquis  TYPE zhrst_efd_e1250m-ind_aquis,
         belnr      TYPE belnr_d, "ZHRST_EFD_E1250M-BELNR,
         xblnr      TYPE zhrst_efd_e1250m-xblnr,
         cpf        TYPE lfa1-stcd1,
         cname      TYPE zhrst_efd_e1250m-cname,
         dmbtr      TYPE zhrst_efd_e1250m-dmbtr,
         vrcp       TYPE zhrst_efd_e1250m-vrcp,
         vrrat      TYPE zhrst_efd_e1250m-vrrat,
         vrsenar    TYPE zhrst_efd_e1250m-vrsenar,
         gjahr      TYPE zhrst_efd_e1250m-gjahr,
         ind_acquis TYPE zhrst_efd_e1250m-ind_acquis,
       END OF ty_zhrst_efd_e1250m.

TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         stcd1 TYPE zhrst_efd_e1250m-cpf,
         stcd2 TYPE zhrst_efd_e1250m-cpf,
       END OF ty_lfa1.

TYPES: BEGIN OF ty_soma,
         bukrs   TYPE zhrst_efd_e1250m-bukrs,
         branch  TYPE zhrst_efd_e1250m-branch,
         cpf     TYPE zhrst_efd_e1250m-cpf,
         dmbtr   TYPE zhrst_efd_e1250m-dmbtr,
         vrcp    TYPE zhrst_efd_e1250m-vrcp,
         vrrat   TYPE zhrst_efd_e1250m-vrrat,
         vrsenar TYPE zhrst_efd_e1250m-vrsenar,
         cname   TYPE zhrst_efd_e1250m-cname,
       END OF ty_soma.


TYPES: BEGIN OF ty_t7brefd_event,
         event_type    TYPE t7brefd_event-event_type,
         begda         TYPE t7brefd_event-begda,
         endda         TYPE t7brefd_event-endda,
         bukrs         TYPE t7brefd_event-bukrs,
         status        TYPE t7brefd_event-status,
         event_receipt TYPE t7brefd_event-event_receipt,
         event_id      TYPE t7brefd_event-event_id,
         inf_value     TYPE t7brefd_event-inf_value,
         branch        TYPE zhrst_efd_e1250m-branch,
       END OF ty_t7brefd_event.

TYPES: ty_rg_belnr TYPE RANGE OF ekbe-belnr,
       ty_rg_gjahr TYPE RANGE OF ekbe-gjahr,
       ty_rg_bukrs TYPE RANGE OF zhrst_efd_e1250m-bukrs.


DATA: t_dd07v TYPE TABLE OF dd07v,
      s_dd07v TYPE dd07v.


DATA: it_saida                TYPE TABLE OF ty_saida,
      wa_saida                TYPE ty_saida,
      it_saida_esocial        TYPE TABLE OF ty_saida_esocial,
      wa_saida_esocial        TYPE ty_saida_esocial,
      it_zhrst_efd_e1250m     TYPE TABLE OF ty_zhrst_efd_e1250m,
      wa_zhrst_efd_e1250m     TYPE ty_zhrst_efd_e1250m,
      it_zhrst_efd_e1250m_aux TYPE TABLE OF ty_zhrst_efd_e1250m,
      wa_zhrst_efd_e1250m_aux TYPE ty_zhrst_efd_e1250m,
      it_lfa1                 TYPE TABLE OF ty_lfa1,
      wa_lfa1                 TYPE ty_lfa1,
      it_t7brefd_event        TYPE TABLE OF t7brefd_event,
      wa_t7brefd_event        TYPE  t7brefd_event,

      it_t7brefd_event_aux    TYPE TABLE OF ty_t7brefd_event,
      wa_t7brefd_event_aux    TYPE  ty_t7brefd_event,

      it_t7brefd_rurlwrkr     TYPE TABLE OF t7brefd_rurlwrkr,
      wa_t7brefd_rurlwrkr     TYPE t7brefd_rurlwrkr,
      it_ekbe                 TYPE TABLE OF ekbe,
      wa_ekbe                 TYPE ekbe,
      it_ekko                 TYPE TABLE OF ekko,
      wa_ekko                 TYPE ekko,
      it_bkpf                 TYPE TABLE OF bkpf,
      wa_bkpf                 TYPE bkpf,
      it_soma                 TYPE TABLE OF ty_soma,
      wa_soma                 TYPE ty_soma.


DATA: g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container1 TYPE REF TO cl_gui_custom_container,
      dg_splitter_1       TYPE REF TO cl_gui_splitter_container,
      dg_parent_1         TYPE REF TO cl_gui_container,
      dg_splitter_2       TYPE REF TO cl_gui_splitter_container,
      dg_parent_2         TYPE REF TO cl_gui_container,
      dg_parent_2a        TYPE REF TO cl_gui_container,
      dg_parent_alv       TYPE REF TO cl_gui_container,
      picture             TYPE REF TO cl_gui_picture,
      gs_layout           TYPE lvc_s_layo,
      gs_layout1          TYPE lvc_s_layo,
      ctl_alv             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog     TYPE TABLE OF lvc_s_fcat,
      wa_fieldcatalog     TYPE lvc_s_fcat,
      gs_scroll_col       TYPE lvc_s_col,
      gs_scroll_row       TYPE lvc_s_roid,
      gs_variant          TYPE disvariant,
      gs_variant1         TYPE disvariant,
      it_exclude_fcode    TYPE ui_functions,
      wa_exclude_fcode    LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id        TYPE REF TO cl_dd_document,
      table_element       TYPE REF TO cl_dd_table_element,
      column              TYPE REF TO cl_dd_area,
      table_element2      TYPE REF TO cl_dd_table_element,
      column_1            TYPE REF TO cl_dd_area,
      column_2            TYPE REF TO cl_dd_area,
      column_3            TYPE REF TO cl_dd_area,
      dg_html_cntrl       TYPE REF TO cl_gui_html_viewer,
      it_header           TYPE kkblo_t_listheader WITH HEADER LINE.   "Cabeçalho

DATA: it_fcat    TYPE TABLE OF lvc_s_fcat.

DATA: dt_inicio(6) TYPE c,
      dt_fim(6)    TYPE c,
      xdoc(10)     TYPE c.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs FOR zhrst_efd_e1250m-bukrs NO INTERVALS  NO-EXTENSION OBLIGATORY,
                p_branch FOR zhrst_efd_e1250m-branch NO INTERVALS,
                p_budat FOR zhrst_efd_e1250m-budat OBLIGATORY,
                p_bldat FOR zhrst_efd_e1250m-bldat,
                p_lifnr FOR lfa1-lifnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
PARAMETERS: anal     RADIOBUTTON GROUP g2 USER-COMMAND modifica_tela DEFAULT 'X',  "Rel Analitico
            rec      RADIOBUTTON GROUP g2, " Rel Reconciliação E-Social
            p_txt(1) TYPE c NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS: prod RADIOBUTTON GROUP g1 MODIF ID t1,  "Produtor - SIGAM
            div  RADIOBUTTON GROUP g1 MODIF ID t1, "Diversos - SAP
            tds  RADIOBUTTON GROUP g1 MODIF ID t1. "Todos
SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN OUTPUT.

  PERFORM modifica_tela.

INITIALIZATION.

START-OF-SELECTION.

  CLEAR: dt_inicio, dt_fim.

  dt_inicio = p_budat-low+0(6).
  dt_fim    = p_budat-high+0(6).

  IF dt_inicio <> dt_fim.
    MESSAGE 'O intervalo de data deverá ser dentro do mês!' TYPE 'I'.
    EXIT.
  ELSE.
    IF anal = 'X'.
      PERFORM busca_dados.
      PERFORM tratar_dados.
    ELSEIF rec  = 'X'.
      PERFORM busca_dados_esocial.
      PERFORM tratar_dados_esocial.
    ENDIF.
    IF it_saida[] IS INITIAL AND it_saida_esocial[] IS INITIAL.
      MESSAGE 'Sem informações para os parâmetros informados' TYPE 'I'.
      EXIT.
    ELSEIF ( p_txt = abap_true ).
      PERFORM exporta_txt.
    ELSE.
      PERFORM imprimi_alv.
    ENDIF.

  ENDIF.

END-OF-SELECTION.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.

FORM modifica_tela.

  LOOP AT SCREEN.
    IF anal = 'X'.
      IF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ELSEIF rec = 'X'.
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.


FORM busca_dados.

  IF p_lifnr IS INITIAL.

    IF p_bldat IS NOT INITIAL.

*      SELECT  BUKRS BRANCH BUDAT BLDAT IND_AQUIS BELNR XBLNR
*              CPF CNAME  DMBTR VRCP  VRRAT VRSENAR  GJAHR IND_ACQUIS
*        FROM ZHRST_EFD_E1250M
*        INTO TABLE IT_ZHRST_EFD_E1250M
*      WHERE BUKRS IN P_BUKRS
*        AND BUDAT BETWEEN P_BUDAT-LOW AND P_BUDAT-HIGH
*        AND BLDAT BETWEEN P_BLDAT-LOW AND P_BLDAT-HIGH.

      SELECT
       z1250~bukrs,
       z1250~branch,
       z1250~budat,
       z1250~bldat,
       z1250~ind_aquis,
       z1250~belnr,
       z1250~xblnr,
       z1250~cpf,
       z1250~cname,
       z1250~dmbtr,
       z1250~vrcp,
       z1250~vrrat,
       z1250~vrsenar,
       z1250~gjahr,
       z1250~ind_acquis
      FROM zhrst_efd_e1250m AS z1250
      INTO TABLE @DATA(t_efd_e1250m)
      WHERE
       z1250~bukrs IN @p_bukrs AND
       z1250~budat BETWEEN @p_budat-low AND @p_budat-high AND
       z1250~bldat BETWEEN @p_bldat-low AND @p_bldat-high.

    ELSE.

*      SELECT  BUKRS BRANCH BUDAT BLDAT IND_AQUIS BELNR XBLNR
*              CPF CNAME  DMBTR VRCP  VRRAT VRSENAR  GJAHR IND_ACQUIS
*        FROM ZHRST_EFD_E1250M
*        INTO TABLE IT_ZHRST_EFD_E1250M
*      WHERE BUKRS IN P_BUKRS
*        AND BUDAT BETWEEN P_BUDAT-LOW AND P_BUDAT-HIGH.

      SELECT
       z1250~bukrs,
       z1250~branch,
       z1250~budat,
       z1250~bldat,
       z1250~ind_aquis,
       z1250~belnr,
       z1250~xblnr,
       z1250~cpf,
       z1250~cname,
       z1250~dmbtr,
       z1250~vrcp,
       z1250~vrrat,
       z1250~vrsenar,
       z1250~gjahr,
       z1250~ind_acquis
      FROM zhrst_efd_e1250m AS z1250
      INTO TABLE @t_efd_e1250m
      WHERE
       z1250~bukrs IN @p_bukrs AND
       z1250~budat BETWEEN @p_budat-low AND @p_budat-high.


    ENDIF.

    IF t_efd_e1250m[] IS NOT INITIAL.

      LOOP AT t_efd_e1250m[] INTO DATA(w_efd_e1250m).

        w_efd_e1250m-belnr = |{ w_efd_e1250m-belnr ALPHA = IN }|.

        DATA(w_efd) = VALUE ty_zhrst_efd_e1250m(
            bukrs      = w_efd_e1250m-bukrs
            branch     = w_efd_e1250m-branch
            budat      = w_efd_e1250m-budat
            bldat      = w_efd_e1250m-bldat
            ind_aquis  = w_efd_e1250m-ind_aquis
            belnr      = w_efd_e1250m-belnr+10(10)
            xblnr      = w_efd_e1250m-xblnr
            cpf        = w_efd_e1250m-cpf
            cname      = w_efd_e1250m-cname
            dmbtr      = w_efd_e1250m-dmbtr
            vrcp       = w_efd_e1250m-vrcp
            vrrat      = w_efd_e1250m-vrrat
            vrsenar    = w_efd_e1250m-vrsenar
            gjahr      = w_efd_e1250m-gjahr
            ind_acquis = w_efd_e1250m-ind_acquis  ).

        APPEND w_efd TO it_zhrst_efd_e1250m[].
        CLEAR: w_efd, w_efd_e1250m.

      ENDLOOP.

    ENDIF.


    IF it_zhrst_efd_e1250m IS NOT INITIAL.
      SELECT *
        FROM bkpf INTO TABLE it_bkpf
        FOR ALL ENTRIES IN it_zhrst_efd_e1250m
      WHERE  bukrs EQ it_zhrst_efd_e1250m-bukrs
        AND  belnr EQ it_zhrst_efd_e1250m-belnr
        AND  gjahr EQ it_zhrst_efd_e1250m-gjahr.
    ENDIF.

  ELSE.

    SELECT lifnr stcd1  stcd2
      FROM lfa1 INTO TABLE it_lfa1
     WHERE lifnr IN p_lifnr.

    IF it_lfa1 IS NOT INITIAL.

      IF p_bldat IS NOT INITIAL.

*        SELECT  BUKRS BRANCH BUDAT BLDAT IND_AQUIS BELNR XBLNR
*                CPF   CNAME  DMBTR VRCP  VRRAT VRSENAR
*                GJAHR IND_ACQUIS
*          FROM ZHRST_EFD_E1250M
*          INTO TABLE IT_ZHRST_EFD_E1250M
*        FOR ALL ENTRIES IN IT_LFA1
*        WHERE BUKRS IN P_BUKRS
*          AND BUDAT BETWEEN P_BUDAT-LOW AND P_BUDAT-HIGH
*          AND BLDAT BETWEEN P_BLDAT-LOW AND P_BLDAT-HIGH
*          AND ( CPF EQ IT_LFA1-STCD1 OR  CPF EQ IT_LFA1-STCD2  ).

        SELECT
         z1250~bukrs,
         z1250~branch,
         z1250~budat,
         z1250~bldat,
         z1250~ind_aquis,
         z1250~belnr,
         z1250~xblnr,
         z1250~cpf,
         z1250~cname,
         z1250~dmbtr,
         z1250~vrcp,
         z1250~vrrat,
         z1250~vrsenar,
         z1250~gjahr,
         z1250~ind_acquis
        FROM zhrst_efd_e1250m AS z1250
        INTO TABLE @t_efd_e1250m
        FOR ALL ENTRIES IN @it_lfa1
        WHERE
         z1250~bukrs IN @p_bukrs AND
         z1250~budat BETWEEN @p_budat-low AND @p_budat-high AND
         z1250~bldat BETWEEN @p_bldat-low AND @p_bldat-high AND
         ( cpf EQ @it_lfa1-stcd1 OR  cpf EQ @it_lfa1-stcd2  ).

      ELSE.

*        SELECT  BUKRS BRANCH BUDAT BLDAT IND_AQUIS BELNR XBLNR
*                CPF   CNAME  DMBTR VRCP  VRRAT VRSENAR
*                GJAHR IND_ACQUIS
*          FROM ZHRST_EFD_E1250M
*          INTO TABLE IT_ZHRST_EFD_E1250M
*        FOR ALL ENTRIES IN IT_LFA1
*        WHERE BUKRS IN P_BUKRS
*          AND BUDAT BETWEEN P_BUDAT-LOW AND P_BUDAT-HIGH
*          AND ( CPF EQ IT_LFA1-STCD1 OR  CPF EQ IT_LFA1-STCD2  ).

        SELECT
         z1250~bukrs,
         z1250~branch,
         z1250~budat,
         z1250~bldat,
         z1250~ind_aquis,
         z1250~belnr,
         z1250~xblnr,
         z1250~cpf,
         z1250~cname,
         z1250~dmbtr,
         z1250~vrcp,
         z1250~vrrat,
         z1250~vrsenar,
         z1250~gjahr,
         z1250~ind_acquis
        FROM zhrst_efd_e1250m AS z1250
        INTO TABLE @t_efd_e1250m
        FOR ALL ENTRIES IN @it_lfa1
        WHERE
         z1250~bukrs IN @p_bukrs AND
         z1250~budat BETWEEN @p_budat-low AND @p_budat-high AND
         ( cpf EQ @it_lfa1-stcd1 OR  cpf EQ @it_lfa1-stcd2  ).

      ENDIF.

      IF t_efd_e1250m[] IS NOT INITIAL.

        LOOP AT t_efd_e1250m[] INTO w_efd_e1250m.

          w_efd_e1250m-belnr = |{ w_efd_e1250m-belnr ALPHA = IN }|.

          w_efd = VALUE ty_zhrst_efd_e1250m(
              bukrs      = w_efd_e1250m-bukrs
              branch     = w_efd_e1250m-branch
              budat      = w_efd_e1250m-budat
              bldat      = w_efd_e1250m-bldat
              ind_aquis  = w_efd_e1250m-ind_aquis
              belnr      = w_efd_e1250m-belnr+10(10)
              xblnr      = w_efd_e1250m-xblnr
              cpf        = w_efd_e1250m-cpf
              cname      = w_efd_e1250m-cname
              dmbtr      = w_efd_e1250m-dmbtr
              vrcp       = w_efd_e1250m-vrcp
              vrrat      = w_efd_e1250m-vrrat
              vrsenar    = w_efd_e1250m-vrsenar
              gjahr      = w_efd_e1250m-gjahr
              ind_acquis = w_efd_e1250m-ind_acquis  ).

          APPEND w_efd TO it_zhrst_efd_e1250m[].
          CLEAR: w_efd, w_efd_e1250m.

        ENDLOOP.

      ENDIF.


      IF it_zhrst_efd_e1250m IS NOT INITIAL.
        SELECT *
          FROM bkpf INTO TABLE it_bkpf
          FOR ALL ENTRIES IN it_zhrst_efd_e1250m
        WHERE  bukrs EQ it_zhrst_efd_e1250m-bukrs
          AND  belnr EQ it_zhrst_efd_e1250m-belnr
          AND  gjahr EQ it_zhrst_efd_e1250m-gjahr.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

FORM busca_dados_esocial.


  SELECT * FROM t7brefd_event INTO TABLE it_t7brefd_event
   WHERE event_type EQ '8050'
   AND   bukrs      IN p_bukrs
   AND   begda      EQ p_budat-low
   AND   endda      EQ p_budat-high.


  IF it_t7brefd_event[] IS NOT INITIAL.
    SELECT * FROM  t7brefd_rurlwrkr INTO TABLE it_t7brefd_rurlwrkr
      FOR ALL ENTRIES IN it_t7brefd_event
     WHERE event_id    EQ it_t7brefd_event-event_id.
  ENDIF.


  IF p_lifnr IS INITIAL.
    IF p_bldat IS NOT INITIAL  AND p_branch IS NOT INITIAL.

      SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
              cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
        FROM zhrst_efd_e1250m
        INTO TABLE it_zhrst_efd_e1250m
      WHERE bukrs  IN p_bukrs
        AND branch IN p_branch
        AND gjahr  EQ p_budat-low+0(4)
        AND budat  BETWEEN p_budat-low AND p_budat-high
        AND bldat  BETWEEN p_bldat-low AND p_bldat-high.

    ELSEIF p_bldat IS INITIAL AND p_branch IS NOT INITIAL.

      SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
              cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
        FROM zhrst_efd_e1250m
        INTO TABLE it_zhrst_efd_e1250m
      WHERE bukrs IN p_bukrs
        AND branch IN p_branch
        AND gjahr  EQ p_budat-low+0(4)
        AND budat BETWEEN p_budat-low AND p_budat-high.

    ELSEIF p_bldat IS NOT INITIAL AND p_branch IS INITIAL.

      SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
              cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
        FROM zhrst_efd_e1250m
        INTO TABLE it_zhrst_efd_e1250m
      WHERE bukrs IN p_bukrs
        AND gjahr  EQ p_budat-low+0(4)
        AND budat BETWEEN p_budat-low AND p_budat-high
        AND bldat BETWEEN p_bldat-low AND p_bldat-high.
    ELSE.
      SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
              cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
        FROM zhrst_efd_e1250m
        INTO TABLE it_zhrst_efd_e1250m
      WHERE bukrs IN p_bukrs
        AND gjahr  EQ p_budat-low+0(4)
        AND budat BETWEEN p_budat-low AND p_budat-high.
    ENDIF.

  ELSE.

    SELECT lifnr stcd1  stcd2
      FROM lfa1 INTO TABLE it_lfa1
     WHERE lifnr IN p_lifnr.

    IF it_lfa1[] IS NOT INITIAL.

      IF p_bldat IS NOT INITIAL  AND p_branch IS NOT INITIAL.

        SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
                cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
          FROM zhrst_efd_e1250m
          INTO TABLE it_zhrst_efd_e1250m
          FOR ALL ENTRIES IN it_lfa1
        WHERE bukrs IN p_bukrs
          AND branch IN p_branch
          AND gjahr  EQ p_budat-low+0(4)
          AND budat BETWEEN p_budat-low AND p_budat-high
          AND bldat BETWEEN p_bldat-low AND p_bldat-high
          AND ( cpf EQ it_lfa1-stcd1 OR  cpf EQ it_lfa1-stcd2  ).

      ELSEIF p_bldat IS INITIAL AND p_branch IS NOT INITIAL.

        SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
                cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
          FROM zhrst_efd_e1250m
          INTO TABLE it_zhrst_efd_e1250m
          FOR ALL ENTRIES IN it_lfa1
        WHERE bukrs IN p_bukrs
          AND branch IN p_branch
          AND gjahr  EQ p_budat-low+0(4)
          AND budat BETWEEN p_budat-low AND p_budat-high
          AND ( cpf EQ it_lfa1-stcd1 OR  cpf EQ it_lfa1-stcd2  ).

      ELSEIF p_bldat IS NOT INITIAL AND p_branch IS INITIAL.

        SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
                cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
          FROM zhrst_efd_e1250m
          INTO TABLE it_zhrst_efd_e1250m
          FOR ALL ENTRIES IN it_lfa1
        WHERE bukrs IN p_bukrs
          AND gjahr  EQ p_budat-low+0(4)
          AND budat BETWEEN p_budat-low AND p_budat-high
          AND bldat BETWEEN p_bldat-low AND p_bldat-high
          AND ( cpf EQ it_lfa1-stcd1 OR  cpf EQ it_lfa1-stcd2  ).
      ELSE.
        SELECT  bukrs branch budat bldat ind_aquis belnr xblnr
                cpf cname  dmbtr vrcp  vrrat vrsenar  gjahr ind_acquis
          FROM zhrst_efd_e1250m
          INTO TABLE it_zhrst_efd_e1250m
          FOR ALL ENTRIES IN it_lfa1
        WHERE bukrs IN p_bukrs
          AND gjahr EQ p_budat-low+0(4)
          AND budat BETWEEN p_budat-low AND p_budat-high
          AND ( cpf EQ it_lfa1-stcd1 OR  cpf EQ it_lfa1-stcd2  ).
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.

FORM tratar_dados.

  SORT it_bkpf[] BY belnr ASCENDING.

  DATA(rg_belnr) = VALUE ty_rg_belnr( FOR lwa_bkpf IN it_bkpf[] (
      sign = 'I'
      option = 'EQ'
      low = lwa_bkpf-awkey+0(10)
      high = lwa_bkpf-awkey+0(10) ) ).
  SORT rg_belnr[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM rg_belnr[] COMPARING low.

  DATA(rg_gjahr) = VALUE ty_rg_gjahr( FOR lwa_bkpf IN it_bkpf[] (
      sign = 'I'
      option = 'EQ'
      low = lwa_bkpf-gjahr
      high = lwa_bkpf-gjahr ) ).
  SORT rg_gjahr[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM rg_gjahr[] COMPARING low.

  DATA(rg_bukrs) = VALUE ty_rg_bukrs( FOR lwa_efd IN it_zhrst_efd_e1250m[] (
      sign = 'I'
      option = 'EQ'
      low = lwa_efd-bukrs
      high = lwa_efd-bukrs ) ).
  SORT rg_bukrs[] BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM rg_bukrs[] COMPARING low.

  IF ( rg_belnr[] IS NOT INITIAL ).
    SELECT  *
      FROM ekbe CLIENT SPECIFIED INTO TABLE @DATA(lit_ekbe)
      WHERE mandt = @sy-mandt AND belnr IN @rg_belnr[].
***        AND gjahr IN @rg_gjahr[].
  ENDIF.

  IF ( lit_ekbe[] IS NOT INITIAL ).
    DELETE it_ekbe[] WHERE gjahr NOT IN rg_gjahr[].
    SORT lit_ekbe[] BY gjahr belnr ASCENDING.

    SELECT *  FROM ekko INTO TABLE @DATA(lit_ekko)
      FOR ALL ENTRIES IN @lit_ekbe[]
         WHERE ebeln EQ @lit_ekbe-ebeln
          AND  bukrs IN @rg_bukrs[].
    IF ( lit_ekko[] IS NOT INITIAL ).
      SORT lit_ekko[] BY bukrs ebeln ASCENDING.
    ENDIF.

    SELECT * FROM ekpo INTO TABLE @DATA(lit_ekpo)
        FOR ALL ENTRIES IN @lit_ekbe[]
         WHERE ebeln EQ @lit_ekbe-ebeln
          AND  bukrs IN @rg_bukrs[].
    IF ( lit_ekpo[] IS NOT INITIAL ).
      SORT lit_ekpo[] BY bukrs ebeln ASCENDING.
    ENDIF.

  ENDIF.


  LOOP AT it_zhrst_efd_e1250m INTO wa_zhrst_efd_e1250m.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY belnr = wa_zhrst_efd_e1250m-belnr BINARY SEARCH.
    IF sy-subrc = 0.

      CLEAR xdoc.
      xdoc = wa_bkpf-awkey+0(10).

***      SELECT  SINGLE *
***        FROM ekbe INTO wa_ekbe
***      WHERE gjahr EQ wa_zhrst_efd_e1250m-gjahr
***        AND belnr EQ xdoc.

***      SELECT SINGLE *  FROM ekko INTO wa_ekko
***       WHERE ebeln EQ wa_ekbe-ebeln
***        AND  bukrs EQ wa_zhrst_efd_e1250m-bukrs.

***      SELECT SINGLE  * FROM ekpo INTO @DATA(wa_ekpo)
***        WHERE ebeln EQ @wa_ekbe-ebeln
***        AND   bukrs EQ @wa_zhrst_efd_e1250m-bukrs.

*     Histórico para o documento de compra
      READ TABLE lit_ekbe[] ASSIGNING FIELD-SYMBOL(<lfs_ekbe>)
        WITH KEY gjahr = wa_zhrst_efd_e1250m-gjahr belnr = xdoc BINARY SEARCH.
      IF ( sy-subrc = 0 ).
        wa_saida-ebeln =  <lfs_ekbe>-ebeln.

*       Cabeçalho do documento de compra
        READ TABLE lit_ekko[] ASSIGNING FIELD-SYMBOL(<lfs_ekko>)
          WITH KEY bukrs = wa_zhrst_efd_e1250m-bukrs  ebeln = <lfs_ekbe>-ebeln BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          wa_saida-bsart = <lfs_ekko>-bsart.
          wa_saida-lifnr = |{ <lfs_ekko>-lifnr ALPHA = OUT }|.
        ENDIF.

*       Item do documento de compras
        READ TABLE lit_ekpo[] ASSIGNING FIELD-SYMBOL(<lfs_ekpo>)
          WITH KEY bukrs = wa_zhrst_efd_e1250m-bukrs  ebeln = <lfs_ekbe>-ebeln BINARY SEARCH.
        IF ( sy-subrc = 0 ).
          wa_saida-matkl = <lfs_ekpo>-matkl.
          wa_saida-matnr = |{ <lfs_ekpo>-matnr ALPHA = OUT }|.
          wa_saida-txz01 = <lfs_ekpo>-txz01.
        ENDIF.

      ENDIF.

      wa_saida-branch       =  wa_zhrst_efd_e1250m-branch.
      wa_saida-cpf          =  wa_zhrst_efd_e1250m-cpf.
      wa_saida-budat        =  wa_zhrst_efd_e1250m-budat.
      wa_saida-bldat        =  wa_zhrst_efd_e1250m-bldat.
      wa_saida-ind_aquis    =  wa_zhrst_efd_e1250m-ind_aquis.
      wa_saida-belnr        =  wa_zhrst_efd_e1250m-belnr.
      wa_saida-xblnr        =  wa_zhrst_efd_e1250m-xblnr.
      wa_saida-cname        =  wa_zhrst_efd_e1250m-cname.
      wa_saida-dmbtr        =  wa_zhrst_efd_e1250m-dmbtr.
      wa_saida-vrcp         =  wa_zhrst_efd_e1250m-vrcp.
      wa_saida-vrrat        =  wa_zhrst_efd_e1250m-vrrat.
      wa_saida-vrsenar      =  wa_zhrst_efd_e1250m-vrsenar.
      wa_saida-ind_acquis   =  wa_zhrst_efd_e1250m-ind_acquis.

      IF prod = 'X'.
        IF wa_saida-ebeln IS INITIAL AND wa_bkpf-blart = 'SI'.
          APPEND wa_saida TO it_saida.
        ELSE.
          IF wa_saida-bsart = 'ZGR'.
            APPEND wa_saida TO it_saida.
          ENDIF.
        ENDIF.

      ELSEIF div  = 'X'.

        IF wa_saida-ebeln IS NOT INITIAL AND wa_bkpf-blart <> 'SI'.
          IF wa_saida-bsart <> 'ZGR'.
            APPEND wa_saida TO it_saida.
          ENDIF.
        ENDIF.

      ELSEIF tds = 'X'.
        APPEND wa_saida TO it_saida.
      ENDIF.

    ENDIF.
    CLEAR: wa_saida,  wa_zhrst_efd_e1250m, wa_ekbe, wa_ekko,  s_dd07v. "wa_ekpo

  ENDLOOP.

ENDFORM.

FORM tratar_dados_esocial.
  DATA: xemp    TYPE t7brefd_event-bukrs,
        xfilial TYPE zhrst_efd_e1250m-branch,
        xf      TYPE zhrst_efd_e1250m-branch,
        vstatus TYPE dd07v-domvalue_l.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'HRPADBR_EFD_STATUS'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  SORT it_zhrst_efd_e1250m BY  bukrs  ASCENDING
                               branch ASCENDING
                               cpf    ASCENDING.


  SORT it_t7brefd_event BY inf_value ASCENDING.

  LOOP AT it_t7brefd_event INTO wa_t7brefd_event WHERE inf_value+5(4) <> xfilial.


***    IF  xfilial <> wa_t7brefd_event-inf_value+5(4).

    xemp    = wa_t7brefd_event-inf_value+0(4).
    xfilial = wa_t7brefd_event-inf_value+5(4).


    LOOP AT it_zhrst_efd_e1250m  INTO wa_zhrst_efd_e1250m WHERE bukrs  = xemp AND
                                                                branch = xfilial.

      wa_soma-dmbtr    =  wa_zhrst_efd_e1250m-dmbtr.
      wa_soma-vrcp     =  wa_zhrst_efd_e1250m-vrcp.
      wa_soma-vrrat    =  wa_zhrst_efd_e1250m-vrrat.
      wa_soma-vrsenar  =  wa_zhrst_efd_e1250m-vrsenar.
      wa_soma-cname    =  wa_zhrst_efd_e1250m-cname.
      wa_soma-branch   =  wa_zhrst_efd_e1250m-branch.
      wa_soma-cpf      =  wa_zhrst_efd_e1250m-cpf.
      wa_soma-bukrs    =  wa_zhrst_efd_e1250m-bukrs.
      COLLECT wa_soma INTO it_soma.

      CLEAR: wa_soma, wa_zhrst_efd_e1250m.
    ENDLOOP.

    CLEAR: wa_t7brefd_event, wa_t7brefd_rurlwrkr.

***    ENDIF.
  ENDLOOP.

  SORT it_t7brefd_event BY inf_value ASCENDING. " DESCENDING.
  SORT it_t7brefd_rurlwrkr BY event_id insc_number ASCENDING.

  LOOP AT it_soma  INTO wa_soma.

    wa_saida_esocial-cname    =  wa_soma-cname.
    wa_saida_esocial-branch   =  wa_soma-branch.
    wa_saida_esocial-cpf      =  wa_soma-cpf.
    wa_saida_esocial-dmbtr    =  wa_soma-dmbtr.
    wa_saida_esocial-vrcp     =  wa_soma-vrcp.
    wa_saida_esocial-vrrat    =  wa_soma-vrrat.
    wa_saida_esocial-vrsenar  =  wa_soma-vrsenar.

    READ TABLE it_t7brefd_event INTO wa_t7brefd_event WITH KEY inf_value+0(4) = wa_soma-bukrs
                                                               inf_value+5(4) = wa_soma-branch BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida_esocial-event_id      = |{ wa_t7brefd_event-event_id ALPHA = OUT }|.

      IF wa_t7brefd_event-retif_receipt IS NOT INITIAL.
        wa_saida_esocial-event_receipt = wa_t7brefd_event-retif_receipt.
      ELSE.
        wa_saida_esocial-event_receipt = wa_t7brefd_event-event_receipt.
      ENDIF.

      CLEAR vstatus.

      vstatus = wa_t7brefd_event-status.

      READ TABLE t_dd07v INTO s_dd07v WITH KEY domvalue_l = vstatus.
      IF sy-subrc = 0.
        wa_saida_esocial-status =  | { s_dd07v-domvalue_l }-{ s_dd07v-ddtext } |.
      ENDIF.
    ENDIF.

    "Somar todos os eventos do e-social

    CLEAR: wa_t7brefd_rurlwrkr.

    LOOP AT it_t7brefd_rurlwrkr  INTO wa_t7brefd_rurlwrkr WHERE event_id    = wa_t7brefd_event-event_id AND
                                                                             insc_number = wa_soma-cpf .


*    READ TABLE it_t7brefd_rurlwrkr INTO wa_t7brefd_rurlwrkr WITH KEY event_id    = wa_t7brefd_event-event_id
*                                                                     insc_number = wa_soma-cpf BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida_esocial-acquisit_amt = wa_saida_esocial-acquisit_amt +  wa_t7brefd_rurlwrkr-acquisit_amt.
        wa_saida_esocial-inss_amount  = wa_saida_esocial-inss_amount + wa_t7brefd_rurlwrkr-inss_amount.
        wa_saida_esocial-senar_amt    =  wa_saida_esocial-senar_amt + wa_t7brefd_rurlwrkr-senar_amt.
        wa_saida_esocial-inss_rat     = wa_saida_esocial-inss_rat + wa_t7brefd_rurlwrkr-inss_rat.
      ENDIF.
    ENDLOOP.
    wa_saida_esocial-vlr_base   = ( wa_saida_esocial-dmbtr    -  wa_saida_esocial-acquisit_amt ).
    wa_saida_esocial-vlr_inss   = ( wa_saida_esocial-vrcp     -  wa_saida_esocial-inss_amount  ).
    wa_saida_esocial-vlr_senar  = ( wa_saida_esocial-vrsenar  -  wa_saida_esocial-senar_amt    ).
    wa_saida_esocial-vlr_rat    = ( wa_saida_esocial-vrrat    -  wa_saida_esocial-inss_rat     ).

    APPEND wa_saida_esocial TO it_saida_esocial.

    CLEAR: wa_soma, wa_saida_esocial, wa_t7brefd_event,
           wa_zhrst_efd_e1250m, wa_t7brefd_rurlwrkr.
  ENDLOOP.

ENDFORM.


FORM imprimi_alv.

  IF anal = 'X'.
    PERFORM preenche_cat USING :

          'BRANCH'              'Filial'                  '06'   ''  ''   ''  ''  ''  '',
          'BUDAT'               'Dt.Lcto'                 '10'   ''  ''   ''  ''  ''  '',
          'BLDAT'               'Dt.Dcto'                 '10'   ''  ''   ''  ''  ''  '',
          'BELNR'               'Doc.Contabil'            '10'   ''  'X'  ''  ''  ''  '',
          'XBLNR'               'Nro.NF'                  '10'   ''  ''   ''  ''  ''  '',
          'CPF'                 'CPF/CNPJ'                '14'   ''  ''   ''  ''  ''  '',
          'LIFNR'               'Código'                  '10'   ''  ''   ''  ''  ''  '',
          'CNAME'               'Nome Fornecedor'         '30'   ''  ''   ''  ''  ''  '',
          'DMBTR'               'Valor Nota'              '10'   ''  ''   ''  ''  ''  '',
          'VRCP'                'Valor Funrural'          '10'   ''  ''   ''  ''  ''  '',
          'VRRAT'               'Valor RAT'               '10'   ''  ''   ''  ''  ''  '',
          'VRSENAR'             'Valor SENAR'             '10'   ''  ''   ''  ''  ''  '',
          'IND_ACQUIS'          'Ind.Opção'               '10'   ''  ''   ''  ''  ''  '',
          'IND_AQUIS'           'Ind.Aquisição'           '10'   ''  ''   ''  ''  ''  '',
          'EBELN'               'Pedido'                  '10'   ''  'X'  ''  ''  ''  '',
          'BSART'               'Tp. Pedido'              '10'   ''  ''   ''  ''  ''  '',
          'MATKL'               'Grp. Mercadoria'         '10'   ''  ''   ''  ''  ''  '',
          'MATNR'               'Material'                '10'   ''  ''   ''  ''  ''  '',
          'TXZ01'               'Desc.Material'           '25'   ''  ''   ''  ''  ''  ''.
    CALL SCREEN 0100.

  ELSEIF rec  = 'X'.
    PERFORM preenche_cat USING :

          'STATUS'              'Status'                  '15'   ''  ''   ''  ''  ''  '',
          'EVENT_ID'            'ES-Id Event'             '10'   ''  ''   ''  ''  ''  '',
          'EVENT_RECEIPT'       'Protocolo'               '40'   ''  ''   ''  ''  ''  '',
          'BRANCH'              'Filial'                  '07'   ''  ''   ''  ''  ''  '',
          'CPF'                 'CPF'                     '12'   ''  ''   ''  ''  ''  '',
          'CNAME'               'Nome'                    '25'   ''  ''   ''  ''  ''  '',
          'DMBTR'               'F-Vlr.Base'              '12'   ''  ''   ''  ''  ''  'C410',
          'VRCP'                'F-Vlr.Inss'              '12'   ''  ''   ''  ''  ''  'C410',
          'VRSENAR'             'F-Vlr.Senar'             '12'   ''  ''   ''  ''  ''  'C410',
          'VRRAT'               'F-Vlr.RAT'               '12'   ''  ''   ''  ''  ''  'C410',
          'ACQUISIT_AMT'        'ES-Vlr.Base'             '12'   ''  ''   ''  ''  ''  'C300',
          'INSS_AMOUNT'         'ES-Vlr.INSS'             '12'   ''  ''   ''  ''  ''  'C300',
          'SENAR_AMT'           'ES-Vlr.SENAR'            '12'   ''  ''   ''  ''  ''  'C300',
          'INSS_RAT'            'ES-Vlr.RAT'              '12'   ''  ''   ''  ''  ''  'C300',
          'VLR_BASE'            'DF-Vlr.Base'             '12'   ''  ''   ''  ''  ''  'C500',
          'VLR_INSS'            'DF-Vlr.INSS'             '12'   ''  ''   ''  ''  ''  'C500',
          'VLR_SENAR'           'DF-Vlr.SENAR'            '12'   ''  ''   ''  ''  ''  'C500',
          'VLR_RAT'             'DF-Vlr.RAT'              '12'   ''  ''   ''  ''  ''  'C500'.

    CALL SCREEN 0101.
  ENDIF.

ENDFORM.



FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_icon)
                        VALUE(p_cor).

  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_campo.
  wa_fieldcatalog-scrtext_m = p_campo.
  wa_fieldcatalog-scrtext_s = p_campo.

  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-hotspot   = p_hot.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-icon      = p_icon.
  wa_fieldcatalog-emphasize = p_cor.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.


FORM user_command_0100  USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE lvc_s_col
                             p_es_eow_no TYPE lvc_s_roid.

  READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
  CASE p_e_column_id-fieldname.
    WHEN 'BELNR'.
      DATA(v_belnr) = wa_saida-belnr+10(10).
      IF wa_saida-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD v_belnr. "WA_SAIDA-BELNR.
        SET PARAMETER ID 'BUK' FIELD p_bukrs-low.
        SET PARAMETER ID 'GJR' FIELD wa_saida-budat+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'EBELN'.
      IF wa_saida-ebeln IS NOT INITIAL.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: url(255)                TYPE c,
        data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i.


  SET PF-STATUS 'STATUS'.
  SET TITLEBAR  'TITULO'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 18.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 60.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv.


    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        is_variant                    = gs_variant
        i_save                        = 'A'
        it_toolbar_excluding          = it_exclude_fcode
      CHANGING
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_style = cl_dd_document=>heading.

    p_text = text-002.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.
    "Empresa
    IF p_bukrs IS NOT INITIAL.
      SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
        WHERE bukrs IN @p_bukrs.

      LOOP AT p_bukrs.
        IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
          EXIT.
        ELSEIF p_bukrs-option EQ 'BT'.

          CONCATENATE 'Empresa:' p_bukrs-low '-' wa_t001-butxt  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_bukrs-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Empresa:' p_bukrs-low  '-' wa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element, wa_t001.
    ENDIF.
    "-----------------
    " fornecedor
    IF p_lifnr IS NOT  INITIAL.
      LOOP AT p_lifnr.
        IF p_lifnr-option NE 'EQ' AND p_lifnr-option NE 'BT'.
          sdydo_text_element = 'Fornecedor: Multiplas Seleções'.
          EXIT.
        ELSEIF p_lifnr-option EQ 'BT'.
          CONCATENATE 'Fornecedor:' p_lifnr-low  INTO sdydo_text_element SEPARATED BY space.
          CONCATENATE sdydo_text_element p_lifnr-high  INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Fornecedor: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Fornecedor:' p_lifnr-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    " Data lançamento
    IF p_budat IS NOT INITIAL.
      CONCATENATE  p_budat-low+6(2)  '.'  p_budat-low+4(2)  '.' p_budat-low(4)  INTO data_ini.
      CONCATENATE  p_budat-high+6(2) '.'  p_budat-high+4(2) '.' p_budat-high(4) INTO data_fim.

      CONCATENATE 'Data Lançamento:  ' data_ini  INTO  sdydo_text_element SEPARATED BY space.
      IF data_fim <> '00.00.0000' .
        CONCATENATE sdydo_text_element data_fim  INTO sdydo_text_element SEPARATED BY space.
      ENDIF.

      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element,  data_ini, data_fim.
    ENDIF.

    " Data documento
    IF p_bldat IS NOT INITIAL.
      CONCATENATE  p_bldat-low+6(2)  '.'  p_bldat-low+4(2)  '.' p_bldat-low(4)  INTO data_ini.
      CONCATENATE  p_bldat-high+6(2) '.'  p_bldat-high+4(2) '.' p_bldat-high(4) INTO data_fim.

      CONCATENATE 'Data Documento:  ' data_ini  INTO  sdydo_text_element SEPARATED BY space.
      IF data_fim <> '00.00.0000' .
        CONCATENATE sdydo_text_element data_fim  INTO sdydo_text_element SEPARATED BY space.
      ENDIF.

      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element,  data_ini, data_fim.
    ENDIF.

    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.
  ENDIF.
ENDMODULE.

FORM fill_gs_variant.
  gs_variant-report     = sy-repid.
  gs_variant-handle     = '0100'.
  gs_variant-log_group  = abap_false.
  gs_variant-username   = abap_false.
  gs_variant-text       = abap_false.
  gs_variant-variant    = abap_false.
ENDFORM.

FORM pega_logo USING nome_logo CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  SET PF-STATUS 'STATUS_01'.
  SET TITLEBAR  'TITULO_01'.

  IF g_custom_container1 IS INITIAL.

    CREATE OBJECT g_custom_container1
      EXPORTING
        container_name              = 'CONTAINER01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container1
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 18.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 60.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        is_variant                    = gs_variant
        i_save                        = 'A'
        it_toolbar_excluding          = it_exclude_fcode
      CHANGING
        it_fieldcatalog               = it_fieldcatalog
        it_outtab                     = it_saida_esocial
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_style = cl_dd_document=>heading.

    p_text = text-005.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.
    "Empresa
    IF p_bukrs IS NOT INITIAL.
      SELECT SINGLE * FROM t001 INTO wa_t001
        WHERE bukrs IN p_bukrs.

      LOOP AT p_bukrs.
        IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
          EXIT.
        ELSEIF p_bukrs-option EQ 'BT'.

          CONCATENATE 'Empresa:' p_bukrs-low '-' wa_t001-butxt  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_bukrs-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Empresa:' p_bukrs-low  '-' wa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element, wa_t001.
    ENDIF.
    "------------------

    IF p_branch IS NOT INITIAL.
      SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
        WHERE werks IN @p_branch.

      LOOP AT p_branch.
        IF p_branch-option NE 'EQ' AND p_bukrs-option NE 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          EXIT.
        ELSEIF p_branch-option EQ 'BT'.

          CONCATENATE 'Filial:' p_branch-low '-' wa_t001w-name1  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_branch-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Filial:' p_branch-low  '-' wa_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element, wa_t001.
    ENDIF.
    " Data lançamento
    IF p_budat IS NOT INITIAL.
      CONCATENATE  p_budat-low+6(2)  '.'  p_budat-low+4(2)  '.' p_budat-low(4)  INTO data_ini.
      CONCATENATE  p_budat-high+6(2) '.'  p_budat-high+4(2) '.' p_budat-high(4) INTO data_fim.

      CONCATENATE 'Data Lançamento:  ' data_ini  INTO  sdydo_text_element SEPARATED BY space.
      IF data_fim <> '00.00.0000' .
        CONCATENATE sdydo_text_element data_fim  INTO sdydo_text_element SEPARATED BY space.
      ENDIF.

      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element,  data_ini, data_fim.
    ENDIF.

    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  EXPORTA_TXT
*&---------------------------------------------------------------------*
*  Gerar TXT de Funrural para importação na Sonda
*----------------------------------------------------------------------*
FORM exporta_txt .
  DATA: it_txt         TYPE TABLE OF string,
        v_dia_mes      TYPE sy-datum,
        vl_filename    TYPE string,
        vl_path        TYPE string,
        vl_fullpath    TYPE string,
        it_saida_copia TYPE SORTED TABLE OF ty_saida
            WITH NON-UNIQUE KEY branch budat cpf ind_acquis ind_aquis
            WITH NON-UNIQUE SORTED KEY efd_key COMPONENTS branch cpf ind_acquis ind_aquis.

  IF ( it_saida[] IS INITIAL ).
    MESSAGE 'Sem informações para exportação' TYPE 'E'.
    EXIT.
  ENDIF.

  SORT it_saida[] BY branch cpf ind_acquis ind_aquis ASCENDING.
  it_saida_copia[] = it_saida[].

  LOOP AT it_saida_copia[] INTO DATA(ls_saida_calc)
    GROUP BY ( branch = ls_saida_calc-branch
               cpf    = ls_saida_calc-cpf
               ind_acquis = ls_saida_calc-ind_acquis
               ind_aquis  = ls_saida_calc-ind_aquis
               size       = GROUP SIZE
               index      = GROUP INDEX ) ASCENDING
             REFERENCE INTO DATA(group_saida).

    DATA(soma_dmbtr) = REDUCE prbetrg( INIT x TYPE prbetrg FOR ls_saida IN it_saida[]
       WHERE ( branch = group_saida->branch AND
               cpf    = group_saida->cpf    AND
               ind_acquis = group_saida->ind_acquis AND
               ind_aquis = group_saida->ind_aquis ) NEXT x = x + ls_saida-dmbtr ).

    DATA(soma_vrcp) = REDUCE prbetrg( INIT x TYPE prbetrg FOR ls_saida IN it_saida[]
       WHERE ( branch = group_saida->branch AND
               cpf    = group_saida->cpf    AND
               ind_acquis = group_saida->ind_acquis AND
               ind_aquis = group_saida->ind_aquis ) NEXT x = x + ls_saida-vrcp ).

    DATA(soma_vrrat) = REDUCE prbetrg( INIT x TYPE prbetrg FOR ls_saida IN it_saida[]
       WHERE ( branch = group_saida->branch AND
               cpf    = group_saida->cpf    AND
               ind_acquis = group_saida->ind_acquis AND
               ind_aquis = group_saida->ind_aquis ) NEXT x = x + ls_saida-vrrat ).

    DATA(soma_vrsenar) = REDUCE prbetrg( INIT x TYPE prbetrg FOR ls_saida IN it_saida[]
       WHERE ( branch = group_saida->branch AND
               cpf    = group_saida->cpf    AND
               ind_acquis = group_saida->ind_acquis AND
               ind_aquis = group_saida->ind_aquis ) NEXT x = x + ls_saida-vrsenar ).


    DATA(ls_modifica_saida) = VALUE ty_saida(
        dmbtr      = soma_dmbtr
        vrcp       = soma_vrcp
        vrrat      = soma_vrrat
        vrsenar    = soma_vrsenar    ).

    MODIFY it_saida[] FROM ls_modifica_saida
        TRANSPORTING dmbtr vrcp vrrat vrsenar
        WHERE branch = group_saida->branch AND
              cpf    = group_saida->cpf    AND
              ind_acquis = group_saida->ind_acquis AND
              ind_aquis = group_saida->ind_aquis.

    CLEAR: soma_dmbtr, soma_vrcp, soma_vrrat, soma_vrsenar, ls_modifica_saida.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_saida[] COMPARING branch cpf ind_acquis ind_aquis.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = p_budat-low
    IMPORTING
      last_day_of_month = v_dia_mes
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  SORT it_zhrst_efd_e1250m[] BY branch ASCENDING.


  LOOP AT it_saida[] ASSIGNING FIELD-SYMBOL(<lfs_saida>).

***    READ TABLE it_filial[] ASSIGNING FIELD-SYMBOL(<lfs_filial>) WITH KEY werks = <lfs_saida>-branch BINARY SEARCH.
    READ TABLE it_zhrst_efd_e1250m[] ASSIGNING FIELD-SYMBOL(<lfs_filial>) WITH KEY branch = <lfs_saida>-branch BINARY SEARCH.

    APPEND INITIAL LINE TO it_txt[] ASSIGNING FIELD-SYMBOL(<lfs_line>).
    <lfs_line> = '|S|'.

    <lfs_line> = |{ <lfs_line> }{ v_dia_mes+6(2) }{ v_dia_mes+4(2) }{ v_dia_mes+0(4) }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    <lfs_line> = |{ <lfs_line> }{ <lfs_filial>-bukrs }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    <lfs_line> = |{ <lfs_line> }{ <lfs_saida>-branch }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    <lfs_line> = |{ <lfs_line> }{ <lfs_saida>-cpf }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    IF ( <lfs_saida>-ind_acquis IS INITIAL ).
      <lfs_saida>-ind_acquis = 'N'.
    ENDIF.
    <lfs_line> = |{ <lfs_line> }{ <lfs_saida>-ind_acquis }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    <lfs_line> = |{ <lfs_line> }{ <lfs_saida>-ind_aquis }|.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    DATA(v_valor) = |{ <lfs_saida>-dmbtr }|.
    REPLACE ALL OCCURRENCES OF '.' IN v_valor WITH ','.
    <lfs_line> = |{ <lfs_line> }{ v_valor }|. CLEAR: v_valor.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    v_valor = |{ <lfs_saida>-vrcp }|.
    REPLACE ALL OCCURRENCES OF '.' IN v_valor WITH ','.
    <lfs_line> = |{ <lfs_line> }{ v_valor }|. CLEAR: v_valor.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.

    v_valor = |{ <lfs_saida>-vrrat }|.
    REPLACE ALL OCCURRENCES OF '.' IN v_valor WITH ','.
    <lfs_line> = |{ <lfs_line> }{ v_valor }|. CLEAR: v_valor.
    CONCATENATE <lfs_line> '|' INTO <lfs_line>.


    v_valor = |{ <lfs_saida>-vrsenar }|.
    REPLACE ALL OCCURRENCES OF '.' IN v_valor WITH ','.
    <lfs_line> = |{ <lfs_line> }{ v_valor }|. CLEAR: v_valor.


  ENDLOOP.

  IF ( it_txt[] IS NOT INITIAL ).

    DATA(v_filename) = |FUNRURAL_{ v_dia_mes+6(2) }{ v_dia_mes+4(2) }{ v_dia_mes+0(4) }|.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title              = 'Funrural Gerar arquivo'
        default_file_name         = v_filename
        default_extension         = 'TXT'
        initial_directory         = 'C:\'
      CHANGING
        filename                  = vl_filename
        path                      = vl_path
        fullpath                  = vl_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = vl_fullpath
        confirm_overwrite       = 'X'
      TABLES
        data_tab                = it_txt[]
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

    ELSE.
      DATA(vl_msg_arq) = | Arquivo '{ vl_filename }' gerado com sucesso! |.
      MESSAGE vl_msg_arq TYPE 'S'.
    ENDIF.

  ENDIF.

ENDFORM.
