* A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...:                                                     *
* Objetivo    ...:                                                     *
* Transação   ...:                                                     *
************************************************************************
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& Igor Sobral                  18.06.2013   Add - Funcao             &*
*&                                           Z_DADOSCLASSIFICACAOLOTE &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Report  ZMMR0006
*&---------------------------------------------------------------------*
REPORT  zmmr0006 .

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
INCLUDE: <icon>.

*&---------------------------------------------------------------------*
*&      Tables
*&---------------------------------------------------------------------*
TABLES: mchb, mara.

*&---------------------------------------------------------------------*
*&      Estrutura
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE   TYPE slis_fieldcat_main.
         INCLUDE   TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zmmt0025,            "ADD - 18.06.2013
         atinn TYPE zmmt0025-atinn,
         atnam TYPE zmmt0025-atnam,
       END OF ty_zmmt0025.

*&---------------------------------------------------------------------*
*&      Tabelas/WA/Variaveis
*&---------------------------------------------------------------------*
DATA: tg_mchb       TYPE TABLE OF mchb WITH HEADER LINE,
      tg_zppt0040   TYPE TABLE OF zppt0040 WITH HEADER LINE,
      tg_zmmt0008   TYPE TABLE OF zmmt0008 WITH HEADER LINE,
      tg_mara       TYPE TABLE OF mara WITH HEADER LINE,
      "TG_MCH1       TYPE TABLE OF MCH1 WITH HEADER LINE,
      tg_ausp       TYPE TABLE OF ausp WITH HEADER LINE,
      tg_matnr      TYPE TABLE OF zmme_cl,      "ADD - 18.06.2013
      tg_return     TYPE TABLE OF zmme_cl,      "ADD - 18.06.2013
      tg_return_aux TYPE TABLE OF zmme_cl,      "ADD - 18.06.2013
      tg_zmmt0025   TYPE TABLE OF ty_zmmt0025,  "ADD - 18.06.2013
      wg_return     TYPE zmme_cl,               "ADD - 18.06.2013
      wg_zmmt0025   TYPE ty_zmmt0025.           "ADD - 18.06.2013

DATA: BEGIN OF tg_mch1 OCCURS 0.
        INCLUDE STRUCTURE mch1.
DATA:   objek TYPE ausp-objek,
      END OF tg_mch1.

DATA: BEGIN OF tg_saida OCCURS 0,
        obs(4),
        werks        TYPE mchb-werks,
        lgort        TYPE mchb-lgort,
        matnr        TYPE mchb-matnr,
        normt        TYPE mara-normt,
        fardos_bloco TYPE sy-tabix,
        fardos_emb   TYPE sy-tabix,
        fardos_saldo TYPE sy-tabix,
        "ftakup TYPE sy-tabix,
        ftotal       TYPE sy-tabix,
        peso         TYPE mchb-clabs,
      END OF tg_saida.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.


*&---------------------------------------------------------------------*
*&      Tela de seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_werks  FOR mchb-werks OBLIGATORY,
                  s_lgort  FOR mchb-lgort,
                  s_matnr  FOR mara-matnr,
                  s_matkl  FOR mara-matkl DEFAULT '700140' NO-EXTENSION NO INTERVALS,
                  s_charg  FOR mchb-charg OBLIGATORY NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*&      Start-of-selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:  iniciar_variaves,
            seleciona_dados,
            organiza_dados,
            imprimir_dados.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM seleciona_dados.
** COMMENT - 18.06.2013
**  DATA: WL_ATINN TYPE AUSP-ATINN.
**
**  SELECT * FROM MCHB
**    INTO TABLE TG_MCHB
**  WHERE WERKS IN S_WERKS
***    AND CHARG IN S_CHARG
**    AND LGORT IN S_LGORT
**    AND ( CSPEM GT 0 OR CLABS GT 0 ).
**
**  IF SY-SUBRC IS INITIAL.
**    SELECT *  FROM MARA
**      INTO TABLE TG_MARA
**      FOR ALL ENTRIES IN TG_MCHB
**    WHERE MATNR EQ TG_MCHB-MATNR
**      AND MATKL IN S_MATKL.
**
**    SELECT * FROM MCH1
**      INTO TABLE TG_MCH1
**      FOR ALL ENTRIES IN TG_MCHB
**    WHERE MATNR EQ TG_MCHB-MATNR
**      AND CHARG EQ TG_MCHB-CHARG.
**
**    IF SY-SUBRC IS INITIAL.
**      LOOP AT TG_MCH1.
**        MOVE: TG_MCH1-CUOBJ_BM TO TG_MCH1-OBJEK.
**        MODIFY TG_MCH1.
**      ENDLOOP.
**
**      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
**        EXPORTING
**          INPUT  = 'SAFRA'
**        IMPORTING
**          OUTPUT = WL_ATINN.
**
**      SELECT * FROM AUSP
**        INTO TABLE TG_AUSP
**        FOR ALL ENTRIES IN TG_MCH1
**      WHERE OBJEK EQ TG_MCH1-OBJEK
**        AND ATINN EQ WL_ATINN
**        AND ATFLV IN S_CHARG.
**    ENDIF.
**  ENDIF.

** ADD - 18.06.2013 - Inicio

  IF ( ( s_matnr[] IS NOT INITIAL ) AND ( s_matkl[] IS NOT INITIAL ) OR s_matnr[] IS INITIAL  AND s_matkl IS INITIAL ) .
    MESSAGE s888(sabapdocu) WITH 'Preencha Material ou Grupo de mercadorias' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ELSE.

    SELECT * FROM mara
      INTO TABLE tg_mara
    WHERE matkl IN s_matkl
      AND matnr IN s_matnr.

    CHECK tg_mara[] IS NOT INITIAL.

    SELECT * FROM mchb
      INTO TABLE tg_mchb
      FOR ALL ENTRIES IN tg_mara
    WHERE matnr EQ tg_mara-matnr
      AND werks IN s_werks
      AND lgort IN s_lgort
*    AND charg IN s_charg
      AND ( clabs > 0 OR cspem > 0 ).

    CHECK tg_mchb[] IS NOT INITIAL.
    SORT: tg_mchb BY matnr werks lgort charg.

    SELECT atinn atnam FROM zmmt0025 INTO TABLE tg_zmmt0025.

    CHECK tg_zmmt0025 IS NOT INITIAL.
    SORT tg_zmmt0025 BY atnam.

    READ TABLE tg_zmmt0025 INTO wg_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.

    LOOP AT tg_mchb.
      MOVE: tg_mchb-matnr     TO wg_return-matnr,
            tg_mchb-charg     TO wg_return-charg,
            wg_zmmt0025-atinn TO wg_return-atinn.

      APPEND wg_return TO tg_matnr.
    ENDLOOP.

    "Projeto Reestruturação Algodao 2024
    IF tg_mchb[] IS NOT INITIAL.
      SELECT *
        FROM zppt0040 INTO TABLE tg_zppt0040
         FOR ALL ENTRIES IN tg_mchb
       WHERE werks       EQ tg_mchb-werks
         AND lgort       EQ tg_mchb-lgort
         AND safra       EQ s_charg-low.

      SELECT *
        FROM zmmt0008 INTO TABLE tg_zmmt0008
         FOR ALL ENTRIES IN tg_mchb
       WHERE werks   EQ tg_mchb-werks
         AND lgort   EQ tg_mchb-lgort
         AND safra   EQ s_charg-low.

    ENDIF.
    "Projeto Reestruturação Algodao 2024

    IF tg_matnr IS NOT INITIAL.
      CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
        TABLES
          t_matnr  = tg_matnr
          t_return = tg_return
        EXCEPTIONS
          erro4    = 1.
*        OTHERS         = 2.
      IF sy-subrc <> 0.
*      sy-subrc = 1 "Dados não encontrados
      ELSE.
        DELETE tg_return WHERE atwrt NE s_charg-low.
*        LOOP AT TG_RETURN INTO WG_RETURN WHERE ATWRT NE S_CHARG-LOW.
*          APPEND WG_RETURN TO TG_RETURN_AUX.
*        ENDLOOP.
*        " Deletar com SAFRA diferente da informada
*        IF TG_RETURN_AUX[] IS NOT INITIAL.
*          CLEAR: WG_RETURN.
*          LOOP AT TG_RETURN_AUX INTO WG_RETURN.
*            DELETE TG_RETURN WHERE MATNR EQ WG_RETURN-MATNR AND CHARG EQ WG_RETURN-CHARG.
*            DELETE TG_MCHB   WHERE MATNR EQ WG_RETURN-MATNR AND CHARG EQ WG_RETURN-CHARG.
*          ENDLOOP.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
** ADD - 18.06.2013 - Fim
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM organiza_dados.
  CHECK tg_mchb IS NOT INITIAL AND tg_return IS NOT INITIAL.
  SORT tg_return BY matnr charg.
  SORT tg_mara BY matnr.
  SORT tg_zppt0040 BY werks lgort safra.

  LOOP AT tg_mchb.
**
    CLEAR: tg_saida.

    READ TABLE tg_mara WITH KEY matnr = tg_mchb-matnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_return INTO wg_return WITH KEY matnr = tg_mchb-matnr
                                                   charg = tg_mchb-charg
                                          BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE: tg_mchb-werks TO tg_saida-werks,
              tg_mchb-lgort TO tg_saida-lgort,
              tg_mchb-matnr TO tg_saida-matnr,
              tg_mara-normt TO tg_saida-normt.

        "Projeto Reestruturação Algodao 2024
        READ TABLE tg_zppt0040 INTO DATA(lwa_zppt0040) WITH KEY werks   = tg_mchb-werks
                                                                lgort   = tg_mchb-lgort
                                                                safra   = s_charg-low BINARY SEARCH.
        IF sy-subrc EQ 0.
          tg_saida-fardos_bloco = lwa_zppt0040-qtd_fardinhos.

          LOOP AT tg_zmmt0008 INTO DATA(lwa_zmmt0008) WHERE werks = lwa_zppt0040-werks
                                                        AND lgort = lwa_zppt0040-lgort
                                                        AND safra = lwa_zppt0040-safra.
            ADD 1 TO tg_saida-fardos_emb.
          ENDLOOP.

          tg_saida-fardos_saldo = tg_saida-fardos_bloco - tg_saida-fardos_emb.

        ELSE.
          "Projeto Reestruturação Algodao 2024
          IF tg_mchb-clabs IS NOT INITIAL.
            ADD: 1            TO tg_saida-fardos_saldo.
            "ADD  1            TO tg_saida-ftotal.
          ENDIF.
        ENDIF.

        "Projeto Reestruturação Algodao 2024
*        IF tg_mchb-cspem IS NOT INITIAL.
*          ADD 1             TO tg_saida-ftakup.
*          ADD 1             TO tg_saida-ftotal.
*        ENDIF.
        "Projeto Reestruturação Algodao 2024

        ADD: tg_mchb-clabs  TO tg_saida-peso,
             tg_mchb-cspem  TO tg_saida-peso.

        COLLECT tg_saida.
      ENDIF.
    ENDIF.

  ENDLOOP.

  LOOP AT tg_saida WHERE ftotal GT 138.
    MOVE: '@1A@' TO tg_saida-obs.
    MODIFY tg_saida.
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM imprimir_dados.

  PERFORM:  definir_eventos,
*            F_ALV_SORT,
            montar_layout.

  SORT tg_saida BY werks.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      it_fieldcat        = estrutura[]
*     IT_SORT            = T_SORT[]
      i_save             = 'A'
      it_events          = events
      is_print           = t_print
    TABLES
      t_outtab           = tg_saida.

ENDFORM.                    " IMPRIMIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.
*                                 SLIS_EV_USER_COMMAND 'XUSER_COMMAND'.

ENDFORM.                    " DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM montar_layout.
  PERFORM montar_estrutura USING:
    1  ' '      ' '       'TG_SAIDA'  'OBS'           'Obs.'              ' ',
    2  'MCHB'   'WERKS'   'TG_SAIDA'  'WERKS'         'Centro'            ' ',
    2  'MCHB'   'LGORT'   'TG_SAIDA'  'LGORT'         'Bloco'             ' ',
    3  'MCHB'   'MATNR'   'TG_SAIDA'  'MATNR'         ' '                 ' ',
    4  'MARA'   'NORMT'   'TG_SAIDA'  'NORMT'         'Tipo'              ' ',
    5  ' '      ' '       'TG_SAIDA'  'FARDOS_BLOCO'  'Fardos Bloco'      ' ',
    6  ' '      ' '       'TG_SAIDA'  'FARDOS_EMB'    'Fardos Embarcados' ' ',
    7  ' '      ' '       'TG_SAIDA'  'FARDOS_SALDO'  'Fardos Livres'     ' ',
    "8  ' '      ' '       'TG_SAIDA'  'FTAKUP'  'Fardos Take-UP'    ' ',
    "9  ' '      ' '       'TG_SAIDA'  'FTOTAL'        'Total de Fardos'   ' ',
    8 'MCHB'   'CLABS'    'TG_SAIDA'  'PESO'          'Peso'              '20'.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname      = p_field.
  wa_estrutura-tabname        = p_tabname.
  wa_estrutura-ref_tabname    = p_ref_tabname.
  wa_estrutura-ref_fieldname  = p_ref_fieldname.
  wa_estrutura-key            = ' '.
  wa_estrutura-key_sel        = 'X'.
  wa_estrutura-col_pos        = p_col_pos.
  wa_estrutura-no_out         = ' '.
  wa_estrutura-seltext_s      = p_scrtext_l.
  wa_estrutura-seltext_m      = p_scrtext_l.
  wa_estrutura-seltext_l      = p_scrtext_l.

  IF p_outputlen IS INITIAL.
    wa_estrutura-outputlen    = x_contador.
  ELSE.
    wa_estrutura-outputlen    = p_outputlen.
  ENDIF.

  APPEND wa_estrutura TO estrutura.
ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = 'CLARO_50'.
ENDFORM. "X_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM iniciar_variaves.

  v_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'H' TEXT-001.

ENDFORM.                    " INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.

  CLEAR ls_line.
  SELECT SINGLE name1 FROM t001w
    INTO ls_line-info
  WHERE werks IN s_werks.

*  IF sy-subrc IS INITIAL.
*    ls_line-key = 'Filial:'.
*    CONCATENATE 'Filial: ' ls_line-info INTO ls_line-info SEPARATED BY space.
*    ls_line-typ  = 'A'.
*    APPEND ls_line TO t_top.
*  ENDIF.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
*  LS_LINE-KEY =
  WRITE sy-datum TO ls_line-info USING EDIT MASK '__/__/____'.
  APPEND ls_line TO t_top.

*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
