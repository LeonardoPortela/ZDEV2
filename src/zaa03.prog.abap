*&---------------------------------------------------------------------*
*& Report  ZAA03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zaa03.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

TYPES: BEGIN OF ty_anlc,
       bukrs TYPE anlc-bukrs,
       anln1 TYPE anlc-anln1,
       anln2 TYPE anlc-anln2,
       gjahr TYPE anlc-gjahr,
       afabe TYPE anlc-afabe,
       kansw TYPE anlc-kansw,
       answl TYPE anlc-answl,
       knafa TYPE anlc-knafa,
       nafag TYPE anlc-nafag,
      END OF ty_anlc,

      BEGIN OF ty_anla,
      bukrs TYPE anla-bukrs,
      anln1 TYPE anla-anln1,
      anln2 TYPE anla-anln2,
      anlkl TYPE anla-anlkl,
      deakt TYPE anla-deakt,
      END OF ty_anla,

      BEGIN OF ty_saida,
       bukrs TYPE anlc-bukrs,
       anln1 TYPE anlc-anln1,
       anln2 TYPE anlc-anln2,
       gjahr TYPE anlc-gjahr,
       anlkl TYPE anla-anlkl,
       deakt TYPE anla-deakt,
       ta_01 TYPE anlc-kansw,
       td_01 TYPE anlc-aufnp,
       ta_02 TYPE anlc-kansw,
       td_02 TYPE anlc-aufnp,
       ta_03 TYPE anlc-kansw,
       td_03 TYPE anlc-aufnp,
       ta_04 TYPE anlc-kansw,
       td_04 TYPE anlc-aufnp,
       ta_05 TYPE anlc-kansw,
       td_05 TYPE anlc-aufnp,
       ta_06 TYPE anlc-kansw,
       td_06 TYPE anlc-aufnp,
       ta_07 TYPE anlc-kansw,
       td_07 TYPE anlc-aufnp,
       ta_08 TYPE anlc-kansw,
       td_08 TYPE anlc-aufnp,
       ta_09 TYPE anlc-kansw,
       td_09 TYPE anlc-aufnp,
       ta_10 TYPE anlc-kansw,
       td_10 TYPE anlc-aufnp,
       ta_11 TYPE anlc-kansw,
       td_11 TYPE anlc-aufnp,
       ta_12 TYPE anlc-kansw,
       td_12 TYPE anlc-aufnp,
      END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELA INTERNAS
*----------------------------------------------------------------------*
DATA: t_anlc TYPE TABLE OF ty_anlc,
      t_anla TYPE TABLE OF ty_anla,
      t_saida TYPE TABLE OF ty_saida.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_anlc  TYPE ty_anlc,
      wa_anla  TYPE ty_anla,
      wa_saida TYPE ty_saida.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.


*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR wa_anlc-bukrs, "OBLIGATORY,
                s_anln1 FOR wa_anlc-anln1,
                s_gjahr FOR wa_anlc-gjahr,
                s_deakt FOR wa_anla-deakt. "NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
PERFORM iniciar_variaves.
PERFORM selecionar_dados.
PERFORM organizacao_dados.
PERFORM imprimir_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados.
  SELECT bukrs anln1 anln2 gjahr afabe kansw answl knafa nafag
  FROM anlc
  INTO TABLE t_anlc
   WHERE bukrs IN s_bukrs
     AND anln1 IN s_anln1
     AND gjahr IN s_gjahr.

  IF sy-subrc IS INITIAL.
    SELECT bukrs anln1 anln2 anlkl deakt
      FROM anla
      INTO TABLE t_anla
       FOR ALL ENTRIES IN t_anlc
       WHERE bukrs EQ t_anlc-bukrs
         AND anln1 EQ t_anlc-anln1
         AND anln2 EQ t_anlc-anln2
         AND deakt IN s_deakt.


  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizacao_dados.
  DATA: wl_field01 TYPE anlc-afabe,
        wl_field02 TYPE anlc-afabe,
        wl_field03 TYPE anlc-afabe,
        wl_field04 TYPE anlc-afabe,
        wl_field05 TYPE anlc-afabe,
        wl_field06 TYPE anlc-afabe,
        wl_field07 TYPE anlc-afabe,
        wl_field08 TYPE anlc-afabe,
        wl_field09 TYPE anlc-afabe,
        wl_field10 TYPE anlc-afabe,
        wl_field11 TYPE anlc-afabe,
        wl_field12 TYPE anlc-afabe,
        wl_tabix   TYPE sy-tabix.

*SORT: T_ANLC BY

  LOOP AT t_anlc INTO wa_anlc.

    wa_saida-bukrs = wa_anlc-bukrs.
    wa_saida-anln1 = wa_anlc-anln1.
    wa_saida-anln2 = wa_anlc-anln2.
    wa_saida-gjahr = wa_anlc-gjahr.

    IF wa_anlc-afabe EQ 1.
      wa_saida-ta_01 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_01 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 2.
      wa_saida-ta_02 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_02 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 5.
      wa_saida-ta_03 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_03 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 10.
      wa_saida-ta_04 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_04 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 11.
      wa_saida-ta_05 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_05 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 15.
      wa_saida-ta_06 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_06 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 16.
      wa_saida-ta_07 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_07 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 20.
      wa_saida-ta_08 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_08 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 30.
      wa_saida-ta_09 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_09 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 41.
      wa_saida-ta_10 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_10 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 42.
      wa_saida-ta_11 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_11 = wa_anlc-knafa + wa_anlc-nafag.
    ELSEIF wa_anlc-afabe EQ 50.
      wa_saida-ta_12 = wa_anlc-kansw + wa_anlc-answl.
      wa_saida-td_12 = wa_anlc-knafa + wa_anlc-nafag.


    ENDIF.
*    IF wl_field01 EQ space
*    OR wl_field01 EQ wa_anlc-afabe.
*      wl_field01 = wa_anlc-afabe.
**
**    ELSEIF wl_field01 EQ space
**    OR wl_field01 EQ wa_anlc-afabe.
**      wl_field01 = wa_anlc-afabe.
*
*      wa_saida-ta_01 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_01 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field02 EQ space
*    OR wl_field02 EQ wa_anlc-afabe.
*      wl_field02 = wa_anlc-afabe.
*
*      wa_saida-ta_02 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_02 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field03 EQ space
*    OR wl_field03 EQ wa_anlc-afabe.
*      wl_field03 = wa_anlc-afabe.
*
*      wa_saida-ta_03 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_03 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field04 EQ space
*    OR wl_field04 EQ wa_anlc-afabe.
*      wl_field04 = wa_anlc-afabe.
*
*      wa_saida-ta_04 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_04 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field05 EQ space
*    OR wl_field05 EQ wa_anlc-afabe.
*      wl_field05 = wa_anlc-afabe.
*
*      wa_saida-ta_05 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_05 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field06 EQ space
*    OR wl_field06 EQ wa_anlc-afabe.
*      wl_field06 = wa_anlc-afabe.
*
*      wa_saida-ta_06 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_06 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field07 EQ space
*    OR wl_field07 EQ wa_anlc-afabe.
*      wl_field07 = wa_anlc-afabe.
*
*      wa_saida-ta_07 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_07 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field08 EQ space
*    OR wl_field08 EQ wa_anlc-afabe.
*      wl_field08 = wa_anlc-afabe.
*
*      wa_saida-ta_08 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_08 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field09 EQ space
*    OR wl_field09 EQ wa_anlc-afabe.
*      wl_field09 = wa_anlc-afabe.
*
*      wa_saida-ta_09 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_09 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field10 EQ space
*    OR wl_field10 EQ wa_anlc-afabe.
*      wl_field10 = wa_anlc-afabe.
*
*      wa_saida-ta_10 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_10 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field11 EQ space
*    OR wl_field11 EQ wa_anlc-afabe.
*      wl_field11 = wa_anlc-afabe.
*
*      wa_saida-ta_11 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_11 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ELSEIF wl_field12 EQ space
*    OR wl_field12 EQ wa_anlc-afabe.
*      wl_field12 = wa_anlc-afabe.
*
*      wa_saida-ta_12 = wa_anlc-kansw + wa_anlc-answl.
*      wa_saida-td_12 = wa_anlc-knafa + wa_anlc-nafag.
*
*    ENDIF.

    COLLECT wa_saida INTO t_saida.
    CLEAR: wa_saida.
  ENDLOOP.

  LOOP AT t_saida INTO wa_saida.
    wl_tabix = sy-tabix.
    READ TABLE t_anla INTO wa_anla
      WITH KEY bukrs = wa_saida-bukrs
               anln1 = wa_saida-anln1
               anln2 = wa_saida-anln2.

    IF sy-subrc IS INITIAL.
      MOVE: wa_anla-anlkl TO wa_saida-anlkl,
            wa_anla-deakt TO wa_saida-deakt.

      MODIFY t_saida FROM wa_saida INDEX wl_tabix.
      CLEAR: wa_anla, wa_saida.
    ELSE.
      DELETE t_saida INDEX wl_tabix.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados.

  PERFORM definir_eventos.
  PERFORM montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
*      i_callback_user_command = 'XUSER_COMMAND'
      it_fieldcat             = estrutura[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    TABLES
      t_outtab                = t_saida.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
*  PERFORM f_carregar_eventos USING:
*                                 slis_ev_user_command 'XUSER_COMMAND',
*                                 slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  PERFORM montar_estrutura USING:
        1 'ANLC'  'BUKRS' 'T_SAIDA' 'BUKRS'  ' '  ' ' ,
        2 'ANLC'  'ANLN1' 'T_SAIDA' 'ANLN1'  ' '  ' ' ,
        3 'ANLC'  'ANLN2' 'T_SAIDA' 'ANLN2'  ' '  ' ' ,
        4 'ANLC'  'GJAHR' 'T_SAIDA' 'GJAHR'  ' '  ' ' ,
        4 'ANLA'  'ANLKL' 'T_SAIDA' 'ANLKL'  ' '  ' ' ,
        4 'ANLA'  'DEAKT' 'T_SAIDA' 'DEAKT'  ' '  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_01'  'T. Aqui 01'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_01'  'T. Depr 01'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_02'  'T. Aqui 02'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_02'  'T. Depr 02'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_03'  'T. Aqui 05'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_03'  'T. Depr 05'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_04'  'T. Aqui 10'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_04'  'T. Depr 10'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_05'  'T. Aqui 11'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_05'  'T. Depr 11'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_06'  'T. Aqui 15'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_06'  'T. Depr 15'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_07'  'T. Aqui 16'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_07'  'T. Depr 16'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_08'  'T. Aqui 20'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_08'  'T. Depr 20'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_09'  'T. Aqui 30'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_09'  'T. Depr 30'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_10'  'T. Aqui 41'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_10'  'T. Depr 41'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_11'  'T. Aqui 42'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_11'  'T. Depr 42'  ' ' ,
        5 ' '     ' '     'T_SAIDA' 'TA_12'  'T. Aqui 50'  ' ' ,
        6 ' '     ' '     'T_SAIDA' 'TD_12'  'T. Depr 50'  ' ' .

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen).

  CLEAR wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.

  ADD 1 TO wa_estrutura-col_pos.       "= p_col_pos.

  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

*    CALL FUNCTION 'reuse_alv_commentary_write'
*      EXPORTING
*        it_list_commentary = t_top
*        i_logo             = 'claro_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaves.

  v_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'h' text-002.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
