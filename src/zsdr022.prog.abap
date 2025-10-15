*&---------------------------------------------------------------------*
*& Report  ZSDR022
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2OJ8 |09/07/2025 |Incluir botão de Reinicializar &*
*&                                    |Status. Chamado: 164255.       &*
*&--------------------------------------------------------------------&*
REPORT zsdr022.

TABLES: zsdt0212, zsdt0214, j_1bnfdoc, j_1bnflin.

TYPES: BEGIN OF ty_saida,
         sts        TYPE char20,
         status     TYPE char20,
         log        TYPE char5,
         style2     TYPE lvc_t_styl,
         docnum     TYPE j_1bnfdoc-docnum,
         status_doc TYPE j_1bnfe_active-docsta,
         cancel     TYPE j_1bnfdoc-cancel,
         direct     TYPE j_1bnfdoc-direct,
         num_nf     TYPE j_1bnfdoc-nfenum,
         pstdat     TYPE j_1bnfdoc-pstdat,
         docdat     TYPE j_1bnfdoc-docdat,
         cretim     TYPE j_1bnfdoc-cretim,
         bukrs      TYPE j_1bnfdoc-bukrs,
         branch     TYPE j_1bnfdoc-branch,
         n_receita  TYPE char100, "Exibir o numero da Receita e colocar link para abrir uma tela com as informações da Receita
         id         TYPE zsdt0212-id,
         id_indea   TYPE zsdt0212-id_indea,
         usnam      TYPE zsdt0212-usnam,
         data_atual TYPE zsdt0212-data_atual,
         hora_atual TYPE zsdt0212-hora_atual,
         nftype     TYPE j_1bnfdoc-nftype,
         cfop       TYPE j_1bnflin-cfop,
         natop      TYPE j_1bnfdoc-natop,
         matkl      TYPE j_1bnflin-matkl,
         wgbez      TYPE t023t-wgbez,
         nome_rtc   TYPE pad_cname,
       END OF ty_saida.


TYPES: BEGIN OF ty_notas,
         docnum     TYPE j_1bnfe_active-docnum,
         docsta     TYPE j_1bnfe_active-docsta,
         cancel     TYPE j_1bnfe_active-cancel,
         pstdat     TYPE j_1bnfdoc-pstdat,
         nfenum     TYPE j_1bnfdoc-nfenum,
         nfnum      TYPE j_1bnfdoc-nfnum,
         series     TYPE j_1bnfdoc-series,
         model      TYPE j_1bnfdoc-model,
         bukrs      TYPE j_1bnfdoc-bukrs,
         branch     TYPE j_1bnfdoc-branch,
         nftype     TYPE j_1bnfdoc-nftype,
         doctyp     TYPE j_1bnfdoc-doctyp,
         cnpj_bupla TYPE j_1bnfdoc-cnpj_bupla,
         cgc        TYPE j_1bnfdoc-cgc,
         direct     TYPE j_1bnfdoc-direct,
         docdat     TYPE j_1bnfdoc-docdat,
         cretim     TYPE j_1bnfdoc-cretim,
         natop      TYPE j_1bnfdoc-natop,
         matnr      TYPE j_1bnflin-matnr,
       END OF ty_notas.

TYPES: BEGIN OF ty_tela_rec,
         dt_inicial TYPE sy-datum,
         dt_final   TYPE sy-datum,
       END OF   ty_tela_rec.


DATA: it_saida     TYPE TABLE OF ty_saida,
      it_saida_aux TYPE TABLE OF ty_saida,
      wa_saida     TYPE  ty_saida,
      it_notas     TYPE TABLE OF ty_notas,
      wa_notas     TYPE  ty_notas,
      it_doc       TYPE TABLE OF ty_notas,
      wa_doc       TYPE  ty_notas,
      it_mara      TYPE TABLE OF mara,
      wa_mara      TYPE mara,
      it_t001w     TYPE TABLE OF t001w,
      it_zsdt0212  TYPE TABLE OF zsdt0212,
      wa_zsdt0212  TYPE  zsdt0212,
      it_zsdt0214  TYPE TABLE OF zsdt0214,
      wa_zsdt0214  TYPE zsdt0214,
      it_zsdt0218  TYPE TABLE OF zsdt0218,
      wa_zsdt0218  TYPE zsdt0218,
      it_lin       TYPE TABLE OF j_1bnflin,
      wa_lin       TYPE j_1bnflin,
      it_t023t     TYPE TABLE OF t023t,
      wa_t023t     TYPE t023t,
      wa_tela      TYPE ty_tela_rec.


DATA: g_custom_container   TYPE REF TO cl_gui_custom_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      tg_selectedrow       TYPE lvc_t_row,
      wg_selectedrow       TYPE lvc_s_row,
      it_fcat              TYPE lvc_t_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      wa_stable            TYPE lvc_s_stbl VALUE 'XX',
      gs_variant           TYPE disvariant,
      l_canc               TYPE c,
      l_nome_rtc           TYPE pad_cname.

DATA: it_texto TYPE STANDARD TABLE OF tline,
      wa_texto TYPE tline,
      tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab,
      wl_name  TYPE thead-tdname,
      wl_cont  TYPE sy-tabix,
      wl_mod   TYPE sy-tabix,
      wl_pos   TYPE sy-tabix,
      wl_line  TYPE sy-tabix.


DATA: r_status TYPE RANGE OF zsdt0212-status,
      w_status LIKE LINE OF r_status.

DATA: it_rsparams TYPE TABLE OF rsparams,
      wa_rsparams TYPE rsparams.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs  FOR j_1bnfdoc-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                  p_branch FOR j_1bnfdoc-branch NO INTERVALS NO-EXTENSION,
                  p_docnum FOR j_1bnfdoc-docnum NO INTERVALS,
                  p_cfop   FOR j_1bnflin-cfop   NO INTERVALS,
                  p_matkl  FOR j_1bnflin-matkl  NO INTERVALS,
                  p_pstdat FOR j_1bnfdoc-pstdat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: s_nenvio RADIOBUTTON GROUP g1 DEFAULT 'X',
              s_anula  RADIOBUTTON GROUP g1,
              s_suces  RADIOBUTTON GROUP g1,
              s_todos  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no sender.

    CLASS-METHODS:
      handle_hotspot_click  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
ENDCLASS.




CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_button_click.

    DATA: hora          TYPE char10,
          data          TYPE char10,
          p_campo_texto TYPE zsdt0214-mensagem.

    READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
    CASE es_col_id.
      WHEN 'LOG'.
        REFRESH tl_texto.

        SELECT * FROM  zsdt0214 INTO TABLE @DATA(tg_zsdt0214)
          WHERE docnum EQ @wa_saida-docnum.

        SORT tg_zsdt0214  DESCENDING BY id data_atual hora_atual.

        LOOP AT tg_zsdt0214 INTO DATA(wg_zsdt0214)
          WHERE docnum = wa_saida-docnum.

          CONCATENATE   wg_zsdt0214-data_atual+6(2) '/' wg_zsdt0214-data_atual+4(2) '/' wg_zsdt0214-data_atual+0(4) INTO data.
          CONCATENATE   wg_zsdt0214-hora_atual+0(2) ':' wg_zsdt0214-hora_atual+2(2) ':' wg_zsdt0214-hora_atual+4(2) INTO hora.

          IF wg_zsdt0214-origem = '1'.

            p_campo_texto =   | { 'SAP' } { '-' } { wg_zsdt0214-mensagem } { '-' } { data } { hora }|.
          ELSE.
            p_campo_texto =   | { 'INDEIA' } { '-' } { wg_zsdt0214-mensagem } { '-' } { data } { hora }|.
          ENDIF.

          wl_cont = strlen( p_campo_texto ).

          WHILE wl_pos < wl_cont.
            wl_line = wl_cont - wl_pos.

            IF wl_line >= 72.
              wl_line = 72.
            ENDIF.

            wl_texto = p_campo_texto+wl_pos(wl_line).
            ADD 72 TO wl_pos.
            IF wl_texto IS NOT INITIAL.
              APPEND wl_texto TO tl_texto.
            ENDIF.
            CLEAR: wl_texto.
          ENDWHILE.
          CLEAR: wg_zsdt0214, wl_texto, wl_cont,  wl_pos, wl_line.
        ENDLOOP.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Log'
            im_display_mode = 'X'
          CHANGING
            ch_text         = tl_texto.

    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD handle_hotspot_click.

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      SET PARAMETER ID 'JEF' FIELD wa_saida-docnum.
      CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.


  IF s_nenvio IS NOT INITIAL.
    w_status-sign   = 'I'.
    w_status-option = 'EQ'.
    w_status-low    = 'E'.
    APPEND w_status TO r_status.
    w_status-low    = 'X'.
    APPEND w_status TO r_status.
    CLEAR w_status.

  ELSEIF s_suces IS NOT INITIAL.

    w_status-sign   = 'I'.
    w_status-option = 'EQ'.
    w_status-low    = 'A'.
    APPEND w_status TO r_status.
    w_status-low    = 'C'.
    APPEND w_status TO r_status.
    CLEAR w_status.

  ELSEIF s_todos IS  NOT INITIAL.

    w_status-sign   = 'I'.
    w_status-option = 'EQ'.
    w_status-low    = 'A'.
    APPEND w_status TO r_status.
    w_status-low    = 'C'.
    APPEND w_status TO r_status.
    w_status-low    = 'E'.
    APPEND w_status TO r_status.
    w_status-low    = 'X'.
    APPEND w_status TO r_status.
    CLEAR w_status.
  ENDIF.


  PERFORM busca_nota.
  PERFORM trata_dados.
  PERFORM alv.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_nota .
  REFRESH it_doc.

  SELECT *
    FROM t001w INTO TABLE it_t001w
     WHERE werks IN p_branch
       AND regio EQ 'MT'.

  IF sy-subrc = 0.

    SELECT  a~docnum, a~docsta,  a~cancel,    b~pstdat, b~nfenum,
            b~nfnum,  b~series, b~model,      b~bukrs,  b~branch,
            b~nftype, b~doctyp, b~cnpj_bupla, b~cgc,    b~direct,
            b~docdat, b~cretim, b~natop
        FROM j_1bnfe_active AS a
        INNER JOIN j_1bnfdoc AS b ON b~docnum = a~docnum
      INTO CORRESPONDING FIELDS OF TABLE @it_notas
       FOR ALL ENTRIES IN @it_t001w
      WHERE a~docnum  IN @p_docnum
       AND  b~bukrs   IN @p_bukrs
       AND  b~branch  EQ @it_t001w-werks
       AND  a~docsta  EQ  '1'
       AND  b~pstdat  IN @p_pstdat
       AND  b~model   IN ('55', '01', '04')
       AND  b~nftype  IN ('NE', 'NF', 'YE', 'YD', 'YF', 'ZA', 'ZB', 'ZD', 'ZH', 'ZL', 'ZO', 'ZR', 'ZT', 'ZU', 'ZV')
       AND  EXISTS ( SELECT docnum FROM j_1bnflin AS c WHERE c~docnum = b~docnum
                                                       AND   c~matkl IN ('700130', '700230', '700240', '700350', '658445') ).

    SELECT  b~docnum, b~docstat,  b~cancel,     b~pstdat, b~nfenum,
            b~nfnum,  b~series,   b~model,      b~bukrs,  b~branch,
            b~nftype, b~doctyp,   b~cnpj_bupla, b~cgc,    b~direct,
            b~docdat, b~cretim,   b~natop
         FROM j_1bnfdoc AS b
        INTO TABLE @it_doc
           FOR ALL ENTRIES IN @it_t001w
         WHERE b~docnum  IN @p_docnum
          AND  b~bukrs   IN @p_bukrs
          AND  b~branch  EQ @it_t001w-werks
          AND  b~pstdat  IN @p_pstdat
          AND  b~nftype  IN ('E1', 'N4')
*          AND  B~DOCSTAT EQ '1'
          AND  b~model   IN ('55', '01', '04')
          AND EXISTS ( SELECT docnum FROM j_1bnflin AS  c WHERE c~docnum = b~docnum
                                                           AND   c~matkl IN ('700130', '700230', '700240', '700350', '658445')   ).

    LOOP AT it_doc INTO wa_doc.
      MOVE wa_doc TO wa_notas.
      APPEND wa_notas TO it_notas.
      CLEAR: wa_doc ,wa_notas.
    ENDLOOP.

    CHECK it_notas IS NOT INITIAL.

    SELECT *
      FROM j_1bnflin INTO TABLE it_lin
      FOR ALL ENTRIES IN it_notas
     WHERE docnum EQ it_notas-docnum
      AND  cfop   IN p_cfop
      AND  matkl  IN p_matkl.

    IF it_lin[] IS NOT INITIAL.
      SELECT *
        FROM mara INTO TABLE it_mara
        FOR ALL ENTRIES IN it_lin
       WHERE matnr EQ it_lin-matnr.
    ENDIF.

    SELECT *
      FROM t023t INTO TABLE it_t023t
      FOR ALL ENTRIES IN it_lin
    WHERE spras EQ sy-langu
      AND matkl EQ it_lin-matkl.

    SELECT *
      FROM zsdt0214 INTO TABLE it_zsdt0214
      FOR ALL ENTRIES IN it_notas
     WHERE docnum EQ it_notas-docnum.

    SELECT * FROM zsdt0212 INTO TABLE it_zsdt0212
      FOR ALL ENTRIES IN it_notas
        WHERE  docnum EQ it_notas-docnum.

  ELSE.
    MESSAGE 'Filial Informada não pertence a região de MT!' TYPE 'I'.
    EXIT.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados.

  DATA vnumeronf TYPE zsdt0218-numeronf.
  DATA vreceita  TYPE char100.

  SORT it_mara BY matnr.

  LOOP AT it_notas INTO wa_notas.

    CLEAR: wa_mara.

    READ TABLE it_lin INTO wa_lin WITH KEY docnum = wa_notas-docnum.
    IF sy-subrc = 0.

      READ TABLE it_zsdt0212 INTO wa_zsdt0212 WITH KEY docnum = wa_notas-docnum.
      IF sy-subrc = 0.

        CASE wa_zsdt0212-status.
          WHEN 'A'.
            CONCATENATE '@08@' ' - Ativo '        INTO wa_saida-status.
          WHEN 'E'.
            CONCATENATE '@0A@' ' - Erro '         INTO wa_saida-status.
          WHEN 'C'.
            CONCATENATE '@09@' ' - Cancelado '    INTO wa_saida-status.
          WHEN 'X'.
            CONCATENATE '@09@' ' - Envio Anulado' INTO wa_saida-status.
        ENDCASE.

        wa_saida-sts         = wa_zsdt0212-status.
        wa_saida-id          = wa_zsdt0212-id.
        wa_saida-id_indea    = wa_zsdt0212-id_indea.
        wa_saida-usnam       = wa_zsdt0212-usnam.
        wa_saida-data_atual  = wa_zsdt0212-data_atual.
        wa_saida-hora_atual  = wa_zsdt0212-hora_atual.
      ENDIF.

      wa_saida-matkl = wa_lin-matkl.
      wa_saida-cfop  = wa_lin-cfop.
      wa_saida-natop = wa_notas-natop.

      READ TABLE it_t023t INTO wa_t023t WITH KEY matkl = wa_lin-matkl.
      IF sy-subrc = 0.
        wa_saida-wgbez = wa_t023t-wgbez.
      ENDIF.

      READ TABLE it_zsdt0214 INTO wa_zsdt0214 WITH KEY docnum = wa_notas-docnum.
      IF sy-subrc = 0 .
        wa_saida-log = '@1E@'.
      ELSE.
        wa_saida-log = '@1F@'.
      ENDIF.

      wa_saida-docnum      = wa_notas-docnum.
      wa_saida-docdat      = wa_notas-docdat.
      wa_saida-pstdat      = wa_notas-pstdat.
      wa_saida-status_doc  = wa_notas-docsta.
      wa_saida-cancel      = wa_notas-cancel.
      wa_saida-direct      = wa_notas-direct.

      IF wa_notas-nfenum IS NOT INITIAL.
        wa_saida-num_nf = wa_notas-nfenum.
      ELSE.
        wa_saida-num_nf = wa_notas-nfnum.
      ENDIF.

      wa_saida-cretim = wa_notas-cretim.
      wa_saida-bukrs  = wa_notas-bukrs.
      wa_saida-branch = wa_notas-branch.

      wa_saida-nftype = wa_notas-nftype.

      IF wa_notas-nfenum IS NOT INITIAL.
        vnumeronf =  |{ wa_notas-nfenum   ALPHA = OUT }|.
      ELSEIF   wa_notas-nfnum IS NOT INITIAL.
        vnumeronf =  |{ wa_notas-nfnum   ALPHA = OUT }|.
      ENDIF.

*-CS2021000218-05.12.2022-#94933-JT-inicio
      FREE: it_zsdt0218.

      SELECT nro_cg, ch_referencia
        INTO @DATA(w_zsdt0001)
        FROM zsdt0001
          UP TO 1 ROWS
       WHERE nro_nf_prod = @wa_notas-docnum.
      ENDSELECT.

      IF sy-subrc = 0.
        SELECT nro_cgd, ch_referencia, id, receitakey
          INTO TABLE @DATA(t_zsdt0298)
          FROM zsdt0298
         WHERE nro_cgd       = @w_zsdt0001-nro_cg
           AND ch_referencia = @w_zsdt0001-ch_referencia
           AND status        = '4'
           AND cancelado     = @abap_false.

        IF sy-subrc = 0.
          SELECT *
            FROM zsdt0218
            INTO TABLE it_zsdt0218
             FOR ALL ENTRIES IN t_zsdt0298
           WHERE receitakey = t_zsdt0298-receitakey
             AND cancelada  = abap_false.
        ENDIF.
      ENDIF.

      IF it_zsdt0218[] IS INITIAL.
        SELECT *
          FROM zsdt0218
          INTO TABLE it_zsdt0218
         WHERE numeronf     EQ vnumeronf
           AND numeropedido EQ wa_notas-branch
           AND cancelada    NE 'X'.
      ENDIF.
*-CS2021000218-05.12.2022-#94933-JT-fim

      CLEAR l_nome_rtc.
      LOOP AT it_zsdt0218 INTO wa_zsdt0218.
        SELECT SINGLE nome
                 INTO l_nome_rtc
                 FROM zsdt0259
                WHERE cpf = wa_zsdt0218-cpfrt.

        IF vreceita IS INITIAL.
          vreceita = wa_zsdt0218-numeroreceita.
        ELSE.
          vreceita = |{ vreceita }/{ wa_zsdt0218-numeroreceita }|.
        ENDIF.
        CLEAR: wa_zsdt0218.
      ENDLOOP.

      IF wa_notas-direct = '2' AND
         wa_lin-matkl = '658445'.
        wa_saida-n_receita = vreceita.
        wa_saida-nome_rtc = l_nome_rtc.
      ENDIF.

      IF     s_nenvio IS NOT INITIAL AND ( wa_zsdt0212-status EQ 'E' OR wa_zsdt0212-status IS INITIAL ).
        APPEND wa_saida TO it_saida.
      ELSEIF s_suces IS NOT INITIAL  AND ( wa_zsdt0212-status EQ 'A' OR wa_zsdt0212-status EQ 'C' ).
        APPEND wa_saida TO it_saida.
      ELSEIF s_anula IS NOT INITIAL  AND wa_zsdt0212-status EQ 'X'.
        APPEND wa_saida TO it_saida.
      ELSEIF s_todos IS NOT INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ENDIF.

    CLEAR: wa_saida, wa_zsdt0212, wa_zsdt0214, vnumeronf, vreceita, wa_lin, wa_t023t.
    REFRESH it_zsdt0218.
  ENDLOOP.


  SORT it_saida BY cfop docnum .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid IS INITIAL AND  g_custom_container IS NOT INITIAL.
      CREATE OBJECT g_grid
        EXPORTING
          i_parent          = g_custom_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    SET HANDLER: lcl_event_handler=>on_button_click FOR g_grid,
                 lcl_event_handler=>handle_hotspot_click FOR g_grid.

    gs_variant-report   = sy-repid.
    gs_variant-username = sy-uname.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
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
    WHEN '&EV_INDEA'.
      PERFORM z_ws_indea.

      REFRESH: it_saida, it_zsdt0214, it_zsdt0212.

      PERFORM: busca_nota,
               trata_dados.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '&REFRESH'.
      REFRESH: it_saida, it_zsdt0214, it_zsdt0212.

      PERFORM: busca_nota,
               trata_dados.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN  '&ATUA_REC'.
*     CLEAR wa_tela.
*     CALL SCREEN 0101 STARTING AT 5 5 ENDING AT 60 10.  "90 10
*----------
* ----Lista receita agronomica
*----------
      CALL FUNCTION 'ZSDMF_BUSCA_RECEITA'.

*** Inicio - Rubenilson Pereira - 10.04.2025 #168932
    WHEN 'MANUAL'.

      PERFORM f_correcao_manual.

      REFRESH: it_saida, it_zsdt0214, it_zsdt0212.

      PERFORM: busca_nota,
               trata_dados.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
**<<<------"184656 - NMS - INI------>>>
    WHEN 'REIN_ST'. "Reinicializar Status
      g_grid->get_selected_rows( IMPORTING et_index_rows = DATA(tl_index_row) ).

      IF tl_index_row[] IS INITIAL.
        MESSAGE |Selecionar a menos uma linha.| TYPE 'S' DISPLAY LIKE 'W'.

      ELSE.
        LOOP AT tl_index_row INTO DATA(el_index_row).
* Atualiza a TI de Saída do ALV.
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX el_index_row-index.
          IF <fs_saida>-status NE '@08@ - Ativo '.
            DATA(vl_st_wrong) = abap_on.
            CONTINUE.

          ENDIF.

          <fs_saida>-status = '@0A@ - Erro '. "Erro.
* Atualiza a TI de consulta da tabela Transparente.
          READ TABLE it_zsdt0212 ASSIGNING FIELD-SYMBOL(<fs_zsdt0212>) WITH KEY docnum = <fs_saida>-docnum.
          <fs_zsdt0212>-status = 'E'. "Erro.
* Atualiza a Tabela Transparente.
          UPDATE zsdt0212 SET status = <fs_zsdt0212>-status WHERE docnum EQ <fs_saida>-docnum.
          COMMIT WORK.
          g_grid->refresh_table_display( EXPORTING is_stable = wa_stable ).

        ENDLOOP.

        IF vl_st_wrong IS INITIAL.
          MESSAGE |Dados atualizados com sucesso.| TYPE 'S'.

        ELSE.
          MESSAGE |Somente os Dados de Status Ativo foram atualizados.| TYPE 'S'.

        ENDIF.

      ENDIF.
**<<<------"184656 - NMS - FIM------>>>
  ENDCASE.
*** Fim - Rubenilson Pereira - 10.04.2025 #168932

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv .

  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING :
        'STATUS'         'Status Envio'           '10'    ''    ''     ''     ''    '',
        'LOG'            'Log'                    '03'    ''    ''     ''     ''    '',
        'DOCNUM'         'Nº Documento'           '10'    ''    'X'    ''     ''    '',
        'STATUS_DOC'     'Status Doc.'            '10'    ''    ''     ''     ''    '',
        'CANCEL'         'Cancelado'              '08'    ''    ''     ''     ''    '',
        'DIRECT'         'Direção'                '10'    ''    ''     ''     ''    '',
        'NUM_NF'         'Nº NF'                  '08'    'X'   ''     ''     ''    '',
        'PSTDAT'         'Dt Lançamento'          '10'    ''    ''     ''     ''    '',
        'DOCDAT'         'Dt Documento'           '10'    ''    ''     ''     ''    '',
        'CRETIM'         'Hora'                   '10'    ''    ''     ''     ''    '',
        'BUKRS'          'Empresa'                '07'    ''    ''     ''     ''    '',
        'BRANCH'         'Loc.Negócio'            '08'    ''    ''     ''     ''    '',
        'N_RECEITA'      'Rec.Agronômico'         '14'    ''    ''     ''     ''    '',
        'NOME_RTC'       'Nome RTC'               '40'    ''    ''     ''     ''    '',
        'CFOP'           'CFOP'                   '14'    ''    ''     ''     ''    '',
        'NATOP'          'Nat. Operação'          '25'    ''    ''     ''     ''    '',
        'MATKL'          'Grp. Mercadoria'        '20'    ''    ''     ''     ''    '',
        'WGBEZ'          'Desc.Grp. Mercadoria'   '21'    ''    ''     ''     ''    '',
        'ID'             'Id'                     '10'    ''    ''     ''     ''    '',
        'ID_INDEA'       'Id Indea'               '10'    ''    ''     ''     ''    '',
        'USNAM'          'Usuário Envio'          '10'    ''    ''     ''     ''    '',
        'DATA_ATUAL'     'Data Envio'             '08'    ''    ''     ''     ''    '',
        'HORA_ATUAL'     'Hora Envio'             '08'    ''    ''     ''     ''    ''.

ENDFORM.


FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor).

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.


  IF p_campo = 'LOG'.
    wl_fcat-style = cl_gui_alv_grid=>mc_style_button.
  ELSE.
    wl_fcat-style = ''.
  ENDIF.

  APPEND wl_fcat TO  it_fcat.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_WS_INDEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_ws_indea .

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow IS INITIAL.
    MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
    EXIT.
  ELSE.

    LOOP AT tg_selectedrow INTO wg_selectedrow.
      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      CLEAR it_rsparams[].
      wa_rsparams-selname = 'P_DOCNUM'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = wa_saida-docnum.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_NFTYPE'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = wa_saida-nftype.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.

      wa_rsparams-selname = 'P_DOCDAT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = wa_saida-docdat.
      APPEND wa_rsparams TO it_rsparams.


      SUBMIT zsdr021 WITH SELECTION-TABLE it_rsparams AND RETURN.

      CLEAR: wa_rsparams, wa_saida, wg_selectedrow.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ST_0101'.
  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.
      IF wa_tela-dt_inicial IS INITIAL.
        MESSAGE 'Favor informe a data inicial' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_tela-dt_final IS INITIAL.
        MESSAGE 'Favor informe a data final' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_tela-dt_final < wa_tela-dt_inicial.
        MESSAGE 'Limite inferior maior que a limite superior' TYPE 'I'.
        EXIT.
      ENDIF.

      DATA(dias) = ( wa_tela-dt_final - wa_tela-dt_inicial ).

      IF dias > 15.
        MESSAGE 'Favor informe um periodo ate 15 dias!' TYPE 'I'.
        EXIT.
      ELSE.
        REFRESH it_rsparams[].
        CLEAR wa_rsparams.

        wa_rsparams-selname = 'P_DT_INI'.
        wa_rsparams-kind    = 'S'.
        wa_rsparams-sign    = 'I'.
        wa_rsparams-option  = 'EQ'.
        wa_rsparams-low     = wa_tela-dt_inicial.
        APPEND wa_rsparams TO it_rsparams.

        wa_rsparams-selname = 'P_DT_FIM'.
        wa_rsparams-kind    = 'S'.
        wa_rsparams-sign    = 'I'.
        wa_rsparams-option  = 'EQ'.
        wa_rsparams-low     = wa_tela-dt_final.
        APPEND wa_rsparams TO it_rsparams.

        wa_rsparams-selname = 'P_EVENTO'.
        wa_rsparams-kind    = 'P'.
        wa_rsparams-sign    = 'I'.
        wa_rsparams-option  = 'EQ'.
        wa_rsparams-low     = '1'.
        wa_rsparams-high    = ' '.
        APPEND wa_rsparams TO it_rsparams.

        SUBMIT zsdr023 WITH SELECTION-TABLE it_rsparams AND RETURN.
        MESSAGE 'Busca feita com Sucesso!' TYPE 'I'.
        LEAVE TO SCREEN 0.

        REFRESH: it_saida, it_zsdt0214, it_zsdt0212.

        PERFORM: busca_nota,
                 trata_dados.

        CALL METHOD g_grid->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*** Inicio - Rubenilson Pereira - 10.04.2025 #168932
*&---------------------------------------------------------------------*
*& Form f_correcao_manual
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_correcao_manual .

  TYPES:
    BEGIN OF ty_indea,
      regio   TYPE j_1bnfe_active-regio,
      nfyear  TYPE j_1bnfe_active-nfyear,
      nfmonth TYPE j_1bnfe_active-nfmonth,
      stcd1   TYPE j_1bnfe_active-stcd1,
      model   TYPE j_1bnfe_active-model,
      serie   TYPE j_1bnfe_active-serie,
      nfnum9  TYPE j_1bnfe_active-nfnum9,
      docnum9 TYPE j_1bnfe_active-docnum9,
      cdv     TYPE j_1bnfe_active-cdv,
      indea   TYPE zsdt0212-id_indea,
    END OF ty_indea.

  DATA: lt_file     TYPE filetable,
        lv_rc       TYPE i,
        lt_upload   TYPE TABLE OF string,
        lv_filename TYPE string,
        lt_indea    TYPE TABLE OF ty_indea,
        lv_chave    TYPE char44,
        lv_indea    TYPE zsdt0212-id_indea,
        lt_zsdt0214 TYPE TABLE OF zsdt0214,
        lt_zsdt0212 TYPE TABLE OF zsdt0212,
        lo_mm_util  TYPE REF TO zcl_mm_util.

  CREATE OBJECT lo_mm_util.

  cl_gui_frontend_services=>file_open_dialog( EXPORTING file_filter      = '*.CSV'
                                                        window_title     = 'Escolha um arquivo'
                                                        default_filename = '*.CSV'
                                              CHANGING  file_table       = lt_file
                                                        rc               = lv_rc ).

  CHECK lt_file IS NOT INITIAL.

  READ TABLE lt_file ASSIGNING FIELD-SYMBOL(<fs_file>) INDEX 1.
  CHECK sy-subrc IS INITIAL.

  lv_filename = <fs_file>-filename.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = lv_filename
      filetype = 'ASC'
    TABLES
      data_tab = lt_upload.

  CHECK lt_upload IS NOT INITIAL.
  DELETE lt_upload INDEX 1.

  LOOP AT lt_upload ASSIGNING FIELD-SYMBOL(<fs_upload>).
    CLEAR: lv_chave, lv_indea.

    APPEND INITIAL LINE TO lt_indea ASSIGNING FIELD-SYMBOL(<fs_indea>).

    REPLACE ALL OCCURRENCES OF '"' IN <fs_upload> WITH space.
    CONDENSE <fs_upload> NO-GAPS.

    IF <fs_upload> CS ','.
      SPLIT <fs_upload> AT ',' INTO lv_chave lv_indea.
    ELSEIF <fs_upload> CS ';'.
      SPLIT <fs_upload> AT ';' INTO lv_chave lv_indea.
    ENDIF.

    <fs_indea> = lv_chave.
    <fs_indea>-indea = lv_indea.

  ENDLOOP.

  SORT lt_indea BY regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE @DATA(lt_active)
    FOR ALL ENTRIES IN @lt_indea
    WHERE regio   = @lt_indea-regio   AND
          nfyear  = @lt_indea-nfyear  AND
          nfmonth = @lt_indea-nfmonth AND
          stcd1   = @lt_indea-stcd1   AND
          model   = @lt_indea-model   AND
          serie   = @lt_indea-serie   AND
          nfnum9  = @lt_indea-nfnum9  AND
          docnum9 = @lt_indea-docnum9 AND
          cdv     = @lt_indea-cdv    .
  IF sy-subrc IS INITIAL.
    SORT lt_active BY docnum.

*          SELECT *
*            FROM ZSDT0212
*            INTO TABLE @DATA(LT_0212)
*            FOR ALL ENTRIES IN @LT_ACTIVE
*            WHERE DOCNUM = @LT_ACTIVE-DOCNUM.
*          IF SY-SUBRC IS INITIAL.


    LOOP AT lt_active ASSIGNING FIELD-SYMBOL(<fs_active>).

      READ TABLE lt_indea ASSIGNING <fs_indea>
      WITH KEY regio   = <fs_active>-regio
               nfyear  = <fs_active>-nfyear
               nfmonth = <fs_active>-nfmonth
               stcd1   = <fs_active>-stcd1
               model   = <fs_active>-model
               serie   = <fs_active>-serie
               nfnum9  = <fs_active>-nfnum9
               docnum9 = <fs_active>-docnum9
               cdv     = <fs_active>-cdv
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        APPEND INITIAL LINE TO lt_zsdt0212 ASSIGNING FIELD-SYMBOL(<fs_0212>).
        <fs_0212>-docnum = <fs_active>-docnum.
        <fs_0212>-status = 'A'.
        <fs_0212>-id_indea = <fs_indea>-indea.
        <fs_0212>-usnam = sy-uname.
        <fs_0212>-data_atual = sy-datum.
        <fs_0212>-hora_atual = sy-uzeit.

        APPEND INITIAL LINE TO lt_zsdt0214 ASSIGNING FIELD-SYMBOL(<fs_0214>).
        <fs_0214>-id           = lo_mm_util->get_next_id_log( ).
        <fs_0214>-data_atual   = sy-datum.
        <fs_0214>-hora_atual   = sy-uzeit.
        <fs_0214>-docnum       = <fs_0212>-docnum.
        <fs_0214>-origem       = '3'.
        <fs_0214>-mensagem     = 'Lançamento Manual Realizado com Sucesso'.
        <fs_0214>-usnam        = sy-uname.

      ENDIF.


    ENDLOOP.

    MODIFY zsdt0212 FROM TABLE lt_zsdt0212.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

    MODIFY zsdt0214 FROM TABLE lt_zsdt0214.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.

    MESSAGE 'Correção realilzada com sucesso' TYPE 'S'.

  ENDIF.

ENDFORM.
*** Fim - Rubenilson Pereira - 10.04.2025 #168932
