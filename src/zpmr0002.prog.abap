*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 23/01/2014                                              &*
*& Descrição: Relatório de Consumo Médio de Combustíveis              &*
*& Transação: ZPMR0002                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zpmr0002.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TABLES: imrg.

TYPE-POOLS: slis, kkblo.

TYPES: BEGIN OF ty_v_equi,
         equnr TYPE v_equi-equnr,
         eqart TYPE v_equi-eqart,
         iwerk TYPE v_equi-iwerk,
         objnr TYPE v_equi-objnr,
         eqktu TYPE v_equi-eqktu,
         swerk TYPE v_equi-swerk,
         daufn TYPE v_equi-daufn,
         typbz TYPE v_equi-typbz,
         herst TYPE v_equi-herst,
         baujj TYPE v_equi-baujj,
         datbi TYPE v_equi-datbi,
         datab TYPE v_equi-datab,
         timbi TYPE v_equi-timbi,
         timei TYPE v_equi-timbi,
       END OF ty_v_equi,

       BEGIN OF ty_imrg,
         point TYPE imrg-point,
         idate TYPE imrg-idate,
         cancl TYPE imrg-cancl,
         mdocm TYPE imrg-mdocm,
         itime TYPE imrg-itime,
         readg TYPE imrg-readg,
         cdiff TYPE imrg-cdiff,
         recdu TYPE imrg-recdu,
       END OF ty_imrg,

       BEGIN OF ty_impt,
         atnan TYPE cabn-atnam,
         point TYPE imptt-point,
         mpobj TYPE imptt-mpobj,
         mrngu TYPE imptt-mrngu,
       END OF ty_impt,

       BEGIN OF ty_t370k_t,
         eqart TYPE v_equi-eqart,
         eartx TYPE t370k_t-eartx,
       END OF ty_t370k_t,

       BEGIN OF ty_001,
         herst      TYPE zpmr0001-herst,
         typbz      TYPE zpmr0001-typbz,
         class_oper TYPE zpmr0001-class_oper,
         consumo    TYPE zpmr0001-consumo,
         variacao   TYPE zpmr0001-variacao,
       END OF ty_001,

       BEGIN OF ty_saida,
         iwerk      TYPE v_equi-iwerk,
         equnr      TYPE v_equi-equnr,
         eqart      TYPE v_equi-eqart,
         categ      TYPE t370k_t-eartx,
         typbz      TYPE v_equi-typbz,
         herst      TYPE v_equi-herst,
         baujj      TYPE v_equi-baujj,
         cdiff      TYPE imrc_totac,
         dt_ini     TYPE imrg-idate,
         t_ini      TYPE imrg-itime,
         dt_fim     TYPE imrg-idate,
         t_fim      TYPE imrg-itime,
         recdu      TYPE imrg-recdu,
         pos_ini    TYPE imrc_totac,
         pos_fim    TYPE imrc_totac,
         pos_inic   TYPE imrc_totac,
         pos_final  TYPE imrc_totac,
         mrngu      TYPE imptt-mrngu,
         total      TYPE p DECIMALS 2,
         media      TYPE p DECIMALS 2,
         consumo    TYPE c LENGTH 8,
         datab      TYPE v_equi-datab,
         datbi      TYPE v_equi-datbi,
         timbi      TYPE v_equi-timbi,
         cell_color TYPE lvc_t_scol,    " Cor da Célula
       END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_inconsistencias,
         iwerk  TYPE v_equi-iwerk,
         iwerk2 TYPE v_equi-swerk,
         equnr  TYPE v_equi-equnr,
         categ  TYPE t370k_t-eartx,
         typbz  TYPE v_equi-typbz,
         herst  TYPE v_equi-herst,
         baujj  TYPE v_equi-baujj,
         idate  TYPE char10,
         itime  TYPE char10,
         l_msg  TYPE c LENGTH 255.
TYPES: END OF ty_inconsistencias.
*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: t_v_equi        TYPE TABLE OF ty_v_equi,
      t_imrg_comb     TYPE TABLE OF ty_imrg,
      t_imrg_comb_aux TYPE TABLE OF ty_imrg,
      t_imrg_cont     TYPE TABLE OF ty_imrg,
      t_imrg_cont_aux TYPE TABLE OF ty_imrg,
      t_impt_comb     TYPE TABLE OF ty_impt,
      t_impt_cont     TYPE TABLE OF ty_impt,
      t_t370k_t       TYPE TABLE OF ty_t370k_t,
      t_001           TYPE TABLE OF ty_001,
      t_saida         TYPE TABLE OF ty_saida,
      t_incons        TYPE TABLE OF ty_inconsistencias.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_v_equi        TYPE ty_v_equi,
      wa_imrg_comb     TYPE ty_imrg,
      wa_imrg_comb_ult TYPE ty_imrg,
      wa_imrg_cont     TYPE ty_imrg,
      wa_impt_comb     TYPE ty_impt,
      wa_impt_cont     TYPE ty_impt,
      wa_t370k_t       TYPE ty_t370k_t,
      wa_001           TYPE ty_001,
      wa_saida         TYPE ty_saida,
      wg_erro          TYPE c,
      wa_incons        TYPE ty_inconsistencias.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events           TYPE          slis_alv_event,
      events              TYPE          slis_t_event,
      t_print             TYPE          slis_print_alv,
      estrutura           TYPE TABLE OF ty_estrutura,
      wa_estrutura        TYPE          ty_estrutura,
      estrutura_incons    TYPE TABLE OF ty_estrutura,
      wa_estrutura_incons TYPE          ty_estrutura,
      v_report            LIKE          sy-repid,
      t_top               TYPE          slis_t_listheader,
      t_sort              TYPE          slis_t_sortinfo_alv,
      wa_sort             TYPE          slis_sortinfo_alv,
      wa_color            TYPE          lvc_s_scol,  " Cor para célula
      it_color            TYPE TABLE OF lvc_s_scol,  " Cor para célula
      init.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_equnr FOR wa_v_equi-equnr,
                  s_docdat FOR imrg-idate NO-EXTENSION OBLIGATORY,
                  s_typbz  FOR wa_v_equi-typbz,
*                s_statio FOR wa_t370-station,
                  s_eqart  FOR wa_v_equi-eqart,
                  s_iwerk  FOR wa_v_equi-iwerk NO-EXTENSION OBLIGATORY.

  PARAMETERS:     p_inatel AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK a1.

SELECTION-SCREEN: BEGIN OF BLOCK a2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_analit RADIOBUTTON GROUP b1,
              p_consol RADIOBUTTON GROUP b1,
              p_segreg RADIOBUTTON GROUP b1.
SELECTION-SCREEN: END OF BLOCK a2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_typbz-low.
  PERFORM busca_modelo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_typbz-high.
  PERFORM busca_modelo.

START-OF-SELECTION.
  PERFORM f_selecionar_dados.
  PERFORM f_organizar_dados.
  PERFORM f_iniciar_variaveis.
  PERFORM f_imprimir_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_dados .
  DATA: wl_return       TYPE bapiret2,
        tl_status_equip TYPE TABLE OF bapi_itob_status WITH HEADER LINE,
        tl_status_user  TYPE TABLE OF bapi_itob_status WITH HEADER LINE,
        t_v_equi_aux    TYPE TABLE OF ty_v_equi,
        wa_v_equi_aux   TYPE ty_v_equi,
        vl_equnr        TYPE v_equi-equnr,
        vl_iwerk        TYPE v_equi-iwerk,
        vl_lines        TYPE i,
        vl_timbi        TYPE v_equi-timbi,
        it_ztparam      TYPE STANDARD TABLE OF ztparam,
        wa_ztparam      TYPE ztparam,
        it_abastec      TYPE RANGE OF ztparam-zval,
        wa_abastec      LIKE LINE OF it_abastec.

  FIELD-SYMBOLS: <fs_v_equi> TYPE ty_v_equi.

  IF s_docdat-high IS INITIAL.
    s_docdat-high = s_docdat-low.
  ENDIF.

  "Seleciona os tipos de equipamentos que permitem abastecimento
  SELECT *
    FROM ztparam
    INTO TABLE it_ztparam
    WHERE param EQ 'TP_OBJ'
    AND abastec EQ abap_true.

  LOOP AT it_ztparam INTO wa_ztparam.
    wa_abastec-sign = 'I'.
    wa_abastec-option = 'EQ'.
    wa_abastec-low = wa_ztparam-zval.
    APPEND wa_abastec TO it_abastec.
  ENDLOOP.

  IF p_consol EQ abap_true.

    SELECT *
      FROM v_equi
      INTO CORRESPONDING FIELDS OF TABLE t_v_equi
      WHERE equnr IN s_equnr
       AND  eqart IN s_eqart
       AND  swerk IN s_iwerk
       AND  typbz IN s_typbz
       AND  eqtyp IN it_abastec
*       AND  ( EQTYP = 'V' OR
*              EQTYP = 'F' ).
       AND datbi EQ '99991231'.

  ELSEIF p_segreg EQ abap_true.

    SELECT *
      FROM v_equi
      INTO CORRESPONDING FIELDS OF TABLE t_v_equi
      WHERE equnr IN s_equnr
      AND  eqart IN s_eqart
      "AND  IWERK IN S_IWERK
      AND  typbz IN s_typbz
      AND  eqtyp IN it_abastec
*      AND  ( EQTYP = 'V' OR
*             EQTYP = 'F' )
      AND ( ( datab IN s_docdat ) OR
            ( datbi IN s_docdat ) OR
            ( datab GT s_docdat-low AND datbi LT s_docdat-high ) ).

  ELSEIF p_analit EQ abap_true.

    SELECT *
      FROM v_equi
      INTO CORRESPONDING FIELDS OF TABLE t_v_equi
      WHERE equnr IN s_equnr
      AND  eqart IN s_eqart
      AND  iwerk IN s_iwerk
      AND  typbz IN s_typbz
*      AND  ( EQTYP = 'V' OR
*             EQTYP = 'F' ).
      AND  eqtyp IN it_abastec.

  ENDIF.

  IF sy-subrc IS INITIAL.
** Pontos de medição
    SELECT atnam point mpobj mrngu
      FROM imptt AS pt
      INNER JOIN cabn AS ca ON ca~atinn = pt~atinn
      INTO CORRESPONDING FIELDS OF TABLE t_impt_comb
      FOR ALL ENTRIES IN t_v_equi
      WHERE mptyp EQ 'M'
       AND  mpobj EQ t_v_equi-objnr
       AND  inact EQ ''
       AND  indtr EQ ''.

    IF p_analit IS NOT INITIAL.
      IF t_impt_comb[] IS NOT INITIAL.
        SELECT a~point a~idate a~cancl a~mdocm a~itime a~readg a~cdiff a~recdu
          FROM imrg AS a
          INTO CORRESPONDING FIELDS OF TABLE t_imrg_comb
           FOR ALL ENTRIES IN t_impt_comb
         WHERE ( ( a~idate GE ( SELECT MAX( g~idate )
                                  FROM imrg AS g
                                 WHERE g~readg NE 0
                                   AND g~idate LT s_docdat-low
                                   AND g~point EQ a~point
                                   AND g~cancl EQ space ) ) OR
                 ( NOT EXISTS ( SELECT *
                                  FROM imrg AS g1
                                 WHERE g1~readg NE 0
                                   AND g1~idate LT s_docdat-low
                                   AND g1~point EQ a~point
                                   AND g1~cancl EQ space ) ) )

           AND  a~idate LE s_docdat-high
           AND  a~cancl EQ space
           AND  a~point EQ t_impt_comb-point.
      ENDIF.

** Ponto de abastecimento
      SELECT atnam point mpobj mrngu
        FROM imptt AS pt
        INNER JOIN cabn AS ca ON ca~atinn = pt~atinn
        INTO CORRESPONDING FIELDS OF TABLE t_impt_cont
        FOR ALL ENTRIES IN t_v_equi
        WHERE mptyp EQ 'V'
         AND  mpobj EQ t_v_equi-objnr
         AND  inact EQ ''
         AND  indtr EQ ''
         AND  ( atnam EQ 'HORIMETRO' OR
                atnam EQ 'ODOMETRO').


      IF t_impt_cont[] IS NOT INITIAL.
        SELECT a~point a~idate a~cancl a~mdocm a~itime a~readg a~cdiff a~recdu
          FROM imrg AS a
          INTO CORRESPONDING FIELDS OF TABLE t_imrg_cont
          FOR ALL ENTRIES IN t_impt_cont
          WHERE ( ( a~idate GE ( SELECT MAX( g~idate )
                              FROM imrg AS g
                             WHERE g~readg NE 0
                               AND g~idate LT s_docdat-low
                               AND g~point EQ a~point
                               AND g~cancl EQ space ) ) OR
            ( NOT EXISTS ( SELECT *
                                  FROM imrg AS g1
                                 WHERE g1~readg NE 0
                                   AND g1~idate LT s_docdat-low
                                   AND g1~point EQ a~point
                                   AND g1~cancl EQ space ) ) )

           AND  idate LE s_docdat-high
           AND  cancl EQ space
           AND  point EQ t_impt_cont-point.
      ENDIF.

    ELSE.
      IF t_impt_comb[] IS NOT INITIAL.
*        SELECT point idate cancl mdocm itime readg cdiff recdu
*          FROM imrg
*          INTO CORRESPONDING FIELDS OF TABLE t_imrg_comb
*          FOR ALL ENTRIES IN t_impt_comb
*          WHERE idate IN s_docdat
*           AND  cancl EQ space
*           AND  point EQ t_impt_comb-point.

        SELECT a~point a~idate a~cancl a~mdocm a~itime a~readg a~cdiff a~recdu
            FROM imrg AS a
            INTO CORRESPONDING FIELDS OF TABLE t_imrg_comb
             FOR ALL ENTRIES IN t_impt_comb
           WHERE ( ( a~idate GE ( SELECT MAX( g~idate )
                                    FROM imrg AS g
                                   WHERE g~readg NE 0
                                     AND g~idate LT s_docdat-low
                                     AND g~point EQ a~point
                                     AND g~cancl EQ space ) ) OR
                   ( NOT EXISTS ( SELECT *
                                    FROM imrg AS g1
                                   WHERE g1~readg NE 0
                                     AND g1~idate LT s_docdat-low
                                     AND g1~point EQ a~point
                                     AND g1~cancl EQ space ) ) )

             AND  a~idate LE s_docdat-high
             AND  a~cancl EQ space
             AND  a~point EQ t_impt_comb-point.
      ENDIF.

** Ponto de abastecimento
      SELECT atnam point mpobj mrngu
        FROM imptt AS pt
        INNER JOIN cabn AS ca ON ca~atinn = pt~atinn
        INTO CORRESPONDING FIELDS OF TABLE t_impt_cont
        FOR ALL ENTRIES IN t_v_equi
        WHERE mptyp EQ 'V'
         AND  mpobj EQ t_v_equi-objnr
         AND  inact EQ ''
         AND  indtr EQ ''
         AND  ( atnam EQ 'HORIMETRO' OR
                atnam EQ 'ODOMETRO').

      IF t_impt_cont[] IS NOT INITIAL.
*        SELECT point idate cancl mdocm itime readg cdiff recdu
*          FROM imrg
*          INTO CORRESPONDING FIELDS OF TABLE t_imrg_cont
*          FOR ALL ENTRIES IN t_impt_cont
*          WHERE idate IN s_docdat
*           AND  cancl EQ space
*           AND  point EQ t_impt_cont-point.

        SELECT a~point a~idate a~cancl a~mdocm a~itime a~readg a~cdiff a~recdu
                  FROM imrg AS a
                  INTO CORRESPONDING FIELDS OF TABLE t_imrg_cont
                  FOR ALL ENTRIES IN t_impt_cont
                  WHERE ( ( a~idate GE ( SELECT MAX( g~idate )
                                      FROM imrg AS g
                                     WHERE g~readg NE 0
                                       AND g~idate LT s_docdat-low
                                       AND g~point EQ a~point
                                       AND g~cancl EQ space ) ) OR
                    ( NOT EXISTS ( SELECT *
                                          FROM imrg AS g1
                                         WHERE g1~readg NE 0
                                           AND g1~idate LT s_docdat-low
                                           AND g1~point EQ a~point
                                           AND g1~cancl EQ space ) ) )

                   AND  idate LE s_docdat-high
                   AND  cancl EQ space
                   AND  point EQ t_impt_cont-point.
      ENDIF.

    ENDIF.
**  Descrição do tipo de objeto
    SELECT *
      FROM t370k_t
      INTO CORRESPONDING FIELDS OF TABLE t_t370k_t
      FOR ALL ENTRIES IN t_v_equi
      WHERE eqart = t_v_equi-eqart
       AND  spras = sy-langu.

**  Selecionando média de cosnumo de equipamentos
    SELECT *
      FROM zpmr0001
      INTO CORRESPONDING FIELDS OF TABLE t_001
      FOR ALL ENTRIES IN t_v_equi
      WHERE herst      = t_v_equi-herst
       AND  typbz      = t_v_equi-typbz
       AND  class_oper = t_v_equi-eqart.

*FF - 30.11.2023 - inicio

    LOOP AT t_001 ASSIGNING FIELD-SYMBOL(<fs_001>) WHERE variacao IS NOT INITIAL.

    ENDLOOP.

*FF - 30.11.2023 - fim


    CHECK t_v_equi IS NOT INITIAL.

    SORT t_v_equi BY equnr.

    IF p_inatel IS INITIAL.
*  * Checando status do equipamento
      LOOP AT t_v_equi ASSIGNING <fs_v_equi>.
        CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
          EXPORTING
            equipment     = <fs_v_equi>-equnr
          IMPORTING
            return        = wl_return
          TABLES
            system_status = tl_status_equip
            user_status   = tl_status_user.

        READ TABLE tl_status_equip WITH KEY status = 'I0076'.
        IF sy-subrc IS INITIAL.
          DELETE t_v_equi WHERE equnr = <fs_v_equi>-equnr.
        ELSE.
          READ TABLE tl_status_equip WITH KEY status = 'I0320'.
          IF sy-subrc IS INITIAL.
            DELETE t_v_equi WHERE equnr = <fs_v_equi>-equnr.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF p_segreg NE abap_true.
      DELETE ADJACENT DUPLICATES FROM t_v_equi COMPARING equnr.
    ELSE.

      t_v_equi_aux = t_v_equi.
      CLEAR: t_v_equi.
      SORT t_v_equi_aux BY equnr datab datbi timbi.

      LOOP AT t_v_equi_aux INTO wa_v_equi_aux.
        IF sy-tabix EQ 1.
          vl_equnr = wa_v_equi_aux-equnr.
          vl_iwerk = wa_v_equi_aux-iwerk.
          APPEND wa_v_equi_aux TO t_v_equi.
        ELSE.
          IF vl_equnr EQ wa_v_equi_aux-equnr AND vl_iwerk EQ wa_v_equi_aux-iwerk.
            DESCRIBE TABLE t_v_equi LINES vl_lines.
            MODIFY t_v_equi FROM wa_v_equi_aux INDEX vl_lines TRANSPORTING datbi timbi.
          ELSE.
            vl_equnr = wa_v_equi_aux-equnr.
            vl_iwerk = wa_v_equi_aux-iwerk.
            APPEND wa_v_equi_aux TO t_v_equi.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: vl_equnr.

      LOOP AT t_v_equi INTO wa_v_equi_aux.
        IF vl_equnr NE wa_v_equi_aux-equnr.
          wa_v_equi_aux-timei = '000000'.
          vl_equnr = wa_v_equi_aux-equnr.
          vl_timbi = wa_v_equi_aux-timbi.
        ELSE.
          wa_v_equi_aux-timei = vl_timbi.
          vl_equnr = wa_v_equi_aux-equnr.
          vl_timbi = wa_v_equi_aux-timbi.
        ENDIF.
        MODIFY t_v_equi FROM wa_v_equi_aux INDEX sy-tabix.
      ENDLOOP.
      "DELETE ADJACENT DUPLICATES FROM T_V_EQUI COMPARING EQUNR IWERK DATAB DATBI.
      DELETE t_v_equi WHERE iwerk NOT IN s_iwerk.
    ENDIF.

  ENDIF.
ENDFORM.                    " SELECIONAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organizar_dados .
  DATA: lv_valor      TYPE imrc_totac,
        lv_consu      TYPE imrc_totac,
        lv_msg        TYPE c LENGTH 255,
        lv_data       TYPE c LENGTH 10,
        lv_hora       TYPE c LENGTH 10,
        lv_pos_atual  TYPE imrc_totac,
        lv_pos_ultim  TYPE imrc_totac,
        lv_data_abast TYPE sy-datum,
        lv_hora_abast TYPE sy-uzeit,
        lv_tabix      TYPE sy-tabix,
        lt_tabix      TYPE sy-tabix,
        wl_imrg       TYPE imrg.

  CLEAR: wa_saida, wa_imrg_comb, wa_imrg_cont, wg_erro.


  IF p_analit IS NOT INITIAL.
    SORT: t_imrg_comb BY point ASCENDING idate DESCENDING itime DESCENDING,
          t_imrg_cont BY point ASCENDING idate DESCENDING itime DESCENDING,
          t_v_equi    BY equnr.
  ELSE.
    SORT: t_imrg_comb BY point idate itime,
          t_imrg_cont BY point idate itime,
          t_v_equi    BY equnr.
  ENDIF.

  LOOP AT  t_v_equi INTO wa_v_equi.
**  Dados do equipamento
    IF p_analit EQ abap_true.
      wa_saida-iwerk = wa_v_equi-iwerk.
    ELSEIF p_consol EQ abap_true.
      wa_saida-iwerk = wa_v_equi-swerk.
    ELSEIF p_segreg EQ abap_true.
      wa_saida-iwerk = wa_v_equi-iwerk.
    ENDIF.

    wa_saida-equnr = wa_v_equi-equnr.
    SHIFT wa_saida-equnr LEFT DELETING LEADING '0'.

    wa_saida-eqart = wa_v_equi-eqart.
    READ TABLE t_t370k_t INTO wa_t370k_t WITH KEY eqart = wa_saida-eqart.
    wa_saida-categ = wa_t370k_t-eartx.
    wa_saida-typbz = wa_v_equi-typbz.
    wa_saida-herst = wa_v_equi-herst.
    wa_saida-baujj = wa_v_equi-baujj.
    READ TABLE t_impt_comb INTO wa_impt_comb WITH KEY mpobj = wa_v_equi-objnr.
    IF sy-subrc IS INITIAL.
**  Dados sobre média de consumo
      CLEAR: wa_001, wa_saida-consumo.
      READ TABLE t_001 INTO wa_001 WITH KEY herst       = wa_v_equi-herst
                                            typbz       = wa_v_equi-typbz
                                            class_oper  = wa_v_equi-eqart.
      IF sy-subrc IS INITIAL.
        wa_saida-consumo  = wa_001-consumo.
      ENDIF.

      IF p_analit IS NOT INITIAL.
        CLEAR lv_tabix.
        LOOP AT t_imrg_comb INTO wa_imrg_comb WHERE point = wa_impt_comb-point.
          lv_tabix = sy-tabix.
**  Dados do abastecimento anterior
          ADD 1 TO lv_tabix.
          CLEAR wa_imrg_comb_ult.
          READ TABLE t_imrg_comb INTO wa_imrg_comb_ult INDEX lv_tabix.
          lv_data_abast = wa_imrg_comb_ult-idate.
          lv_hora_abast = wa_imrg_comb_ult-itime.

          IF wa_imrg_comb-recdu IS INITIAL.
            CONTINUE.
          ENDIF.

**  Dados de abastecimento
          wa_saida-dt_ini = wa_imrg_comb-idate.                                   " Campo data abast.
          wa_saida-t_ini  = wa_imrg_comb-itime.                                   " Campo hora abast.

          CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
            EXPORTING
              char_unit       = wa_imrg_comb-recdu
              decimals        = 0
              exponent        = 0
              fltp_value_si   = wa_imrg_comb-cdiff                                " Campo Consumo
              indicator_value = 'X'
              masc_symbol     = ' '
            IMPORTING
              char_value      = wa_saida-cdiff.

**  Dados do contador
          READ TABLE t_impt_cont INTO wa_impt_cont WITH KEY mpobj = wa_v_equi-objnr.
**  Posição no abastecimento
          lv_pos_ultim = 0.
          lv_pos_atual = 0.
          READ TABLE t_imrg_cont INTO wa_imrg_cont WITH KEY idate = wa_saida-dt_ini
                                                            itime = wa_saida-t_ini
                                                            point = wa_impt_cont-point.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
              EXPORTING
                char_unit       = wa_imrg_cont-recdu
                decimals        = 0
                exponent        = 0
                fltp_value_si   = wa_imrg_cont-readg                            " Campo hori./odo.
                indicator_value = 'X'
                masc_symbol     = ' '
              IMPORTING
                char_value      = lv_pos_atual.
            wa_saida-pos_final = lv_pos_atual.

            CLEAR: wa_imrg_cont.

**  Medição no abastecimento anterior
            READ TABLE t_imrg_cont INTO wa_imrg_cont WITH KEY idate = lv_data_abast
                                                              itime = lv_hora_abast
                                                              point = wa_impt_cont-point.
            IF sy-subrc IS INITIAL.
              CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                EXPORTING
                  char_unit       = wa_imrg_cont-recdu
                  decimals        = 0
                  exponent        = 0
                  fltp_value_si   = wa_imrg_cont-readg                            " Campo hori./odo.
                  indicator_value = 'X'
                  masc_symbol     = ' '
                IMPORTING
                  char_value      = lv_pos_ultim.

              wa_saida-pos_inic = lv_pos_ultim.
            ELSE.
              CLEAR: wl_imrg.
*  Encontra último documento de abastecimento anterior a range da seleção
              PERFORM f_medicao_anterior_comb USING wa_imrg_comb-point
                                                    wa_saida-dt_ini
                                                    wa_saida-t_ini
                                                    wa_imrg_comb-mdocm
                                           CHANGING wl_imrg.

              IF wl_imrg-readg IS INITIAL.
*                LV_DATA_ABAST = WL_IMRG-IDATE.
*                LV_HORA_ABAST = WL_IMRG-ITIME.

*                CLEAR:  WA_SAIDA-POS_FINAL.
**  Encontra ponto de medição anterior a range da seleção
                PERFORM f_medicao_anterior_cont USING wa_impt_cont-point
                                                      lv_data_abast
                                                      lv_hora_abast
                                                      wa_imrg_cont-mdocm
                                             CHANGING wl_imrg.
                IF wl_imrg IS NOT INITIAL.
                  CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                    EXPORTING
                      char_unit       = wl_imrg-recdu
                      decimals        = 0
                      exponent        = 0
                      fltp_value_si   = wl_imrg-readg                            " Campo hori./odo.
                      indicator_value = 'X'
                      masc_symbol     = ' '
                    IMPORTING
                      char_value      = lv_pos_ultim.
                  wa_saida-pos_inic = lv_pos_ultim.
                ELSE.
**  Caso não encontre a medição compátivel com abastecimento anterior zera a média
                  lv_pos_ultim = lv_pos_atual.
                  wa_saida-pos_inic = lv_pos_ultim.
                ENDIF.
*
              ELSE.

**  Caso não haja abastecimento anterior (Equipamento novo) ignora apontamento.
                CLEAR:  wa_imrg_comb, wa_imrg_cont, wa_impt_comb, wa_impt_cont,wa_saida-dt_ini,
                        wa_saida-t_ini, wa_saida-cdiff, wa_saida-pos_ini, wa_saida-media,
                        wa_saida-mrngu, wa_saida-pos_inic,wa_saida-pos_final.
                CONTINUE.
              ENDIF.

            ENDIF.
          ENDIF.

          wa_saida-pos_ini = lv_pos_atual - lv_pos_ultim.

**  Média de consumo
          TRY.
              IF wa_impt_cont-mrngu = 'KM'.
                lv_valor = ( wa_saida-pos_ini / wa_saida-cdiff ).                 " Campo Média Consumo
                wa_saida-media = lv_valor.
              ELSEIF wa_impt_cont-mrngu = 'H'.
                lv_valor = ( wa_saida-cdiff / wa_saida-pos_ini ).                 " Campo Média Consumo
                wa_saida-media = lv_valor.
              ENDIF.
            CATCH cx_root.

          ENDTRY.

**  Tipo do contador
          wa_saida-mrngu = wa_impt_cont-mrngu.

**  Validar cor do registro de acordo com a média de consumo
          IF wa_001-variacao IS NOT INITIAL.
            REFRESH it_color.

**  Cálculo percentual de tolerância
            TRANSLATE wa_001-consumo USING ',.'.
            CONDENSE wa_001-consumo NO-GAPS.
            MOVE wa_001-consumo TO lv_consu.

            IF wa_saida-mrngu = 'KM'.
              lv_valor = lv_consu - ( ( wa_001-variacao / 100 ) * lv_consu ).
              IF wa_saida-media <= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
              wa_saida-cell_color[] = it_color[].
            ELSEIF wa_saida-mrngu = 'H'.
              lv_valor = lv_consu + ( ( wa_001-variacao / 100 ) * lv_consu ).
              IF wa_saida-media >= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
            ENDIF.

            wa_saida-cell_color[] = it_color[].
          ENDIF.


          APPEND: wa_saida TO t_saida.

          CLEAR: wa_imrg_comb, wa_imrg_cont, wa_impt_comb, wa_impt_cont, wa_saida-dt_ini,
                  wa_saida-t_ini, wa_saida-cdiff, wa_saida-pos_ini, wa_saida-media,
                  wa_saida-mrngu, wa_saida-pos_inic,wa_saida-pos_final.

        ENDLOOP.

      ELSEIF p_consol IS NOT INITIAL.
**   Gera relatório CONSOLIDADO
        LOOP AT t_imrg_comb INTO wa_imrg_comb WHERE point = wa_impt_comb-point.
          IF wa_imrg_comb-recdu IS INITIAL.
            CONTINUE.
          ENDIF.

*        ADD 1 TO LT_TABIX.

**  Posição inicial do equipamento no período
          IF wa_saida-dt_ini IS INITIAL.
            READ TABLE t_imrg_cont INTO wa_imrg_cont WITH KEY idate = wa_imrg_comb-idate
                                                              itime = wa_imrg_comb-itime.
*
            IF sy-subrc IS INITIAL.
              READ TABLE t_impt_cont INTO wa_impt_cont WITH KEY point = wa_imrg_cont-point.
              IF sy-subrc IS INITIAL.
                wa_saida-dt_ini = wa_imrg_comb-idate.
                wa_saida-t_ini  = wa_imrg_comb-itime.


                CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                  EXPORTING
                    char_unit       = wa_imrg_cont-recdu
                    decimals        = 0
                    exponent        = 0
                    fltp_value_si   = wa_imrg_cont-readg
*                   FLTP_VALUE_SI   = WA_IMRG_CONT-CDIFF
                    indicator_value = 'X'
                    masc_symbol     = ' '
                  IMPORTING
                    char_value      = lv_valor.

                wa_saida-pos_ini = lv_valor.

              ENDIF.
            ENDIF.
          ENDIF.

          IF wa_imrg_comb-idate GE s_docdat-low.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
              EXPORTING
                char_unit       = wa_imrg_comb-recdu
                decimals        = 0
                exponent        = 0
                fltp_value_si   = wa_imrg_comb-cdiff
                indicator_value = 'X'
                masc_symbol     = ' '
              IMPORTING
                char_value      = lv_valor.

            ADD lv_valor TO wa_saida-cdiff.
          ENDIF.                                               "Campo Consumo

***  Campo total
          READ TABLE t_impt_cont INTO wa_impt_cont WITH KEY mpobj = wa_v_equi-objnr.

          READ TABLE t_imrg_cont INTO wa_imrg_cont WITH KEY idate = wa_imrg_comb-idate
                                                            itime = wa_imrg_comb-itime
                                                            point = wa_imrg_cont-point.
*          IF SY-SUBRC IS INITIAL.
*            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
*              EXPORTING
*                CHAR_UNIT       = WA_IMRG_CONT-RECDU
*                DECIMALS        = 0
*                EXPONENT        = 0
*                FLTP_VALUE_SI   = WA_IMRG_CONT-CDIFF
*                INDICATOR_VALUE = 'X'
*                MASC_SYMBOL     = ' '
*              IMPORTING
*                CHAR_VALUE      = LV_VALOR.
*
*            ADD LV_VALOR TO WA_SAIDA-TOTAL.                                             "Campo total
*
*          ELSE.
          IF sy-subrc IS NOT INITIAL.

            CONCATENATE wa_imrg_comb-idate+6(2) wa_imrg_comb-idate+4(2) wa_imrg_comb-idate+0(4) INTO lv_data SEPARATED BY '.'.
            CONCATENATE wa_imrg_comb-itime+0(2) wa_imrg_comb-itime+2(2) wa_imrg_comb-itime+4(2) INTO lv_hora SEPARATED BY ':'.
            CONCATENATE 'Erro com o equipamento: ' wa_saida-equnr ', abastecimento para o dia ' lv_data ' às ' lv_hora
                        'não possui Doc. de medição. É necessário corrigir para gerar o relatório consolidado.'
                        INTO lv_msg SEPARATED BY space.

            IF wg_erro IS INITIAL.
*              MESSAGE LV_MSG TYPE 'I'.
              wa_incons-l_msg = lv_msg.
              MOVE lv_data TO wa_incons-idate.
              MOVE lv_hora TO wa_incons-itime.
              MOVE wa_v_equi-iwerk TO wa_incons-iwerk2.
              MOVE-CORRESPONDING wa_saida TO wa_incons.
              APPEND wa_incons TO t_incons.
*              WG_ERRO = 'X'.
            ENDIF.

          ENDIF.
        ENDLOOP.

***  Posição final para equipamento no período
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            char_unit       = wa_imrg_cont-recdu
            decimals        = 0
            exponent        = 0
            fltp_value_si   = wa_imrg_cont-readg
            indicator_value = 'X'
            masc_symbol     = ' '
          IMPORTING
            char_value      = lv_valor
          EXCEPTIONS
            no_unit_given   = 1
            unit_not_found  = 2
            OTHERS          = 3.
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.


*        ENDLOOP.

        wa_saida-pos_fim = lv_valor.
        wa_saida-total   = ( wa_saida-pos_fim - wa_saida-pos_ini ).
        wa_saida-dt_fim  = wa_imrg_comb-idate.
        wa_saida-t_fim   = wa_imrg_comb-itime.

        TRY.
            IF wa_impt_cont-mrngu = 'KM'.
              lv_valor = ( wa_saida-total / wa_saida-cdiff ).
              wa_saida-media = lv_valor.
            ELSEIF wa_impt_cont-mrngu = 'H'.
              lv_valor = ( wa_saida-cdiff / wa_saida-total ).
              wa_saida-media = lv_valor.
            ENDIF.
          CATCH cx_root.

        ENDTRY.

**  Tipo do contador
        wa_saida-mrngu = wa_impt_cont-mrngu.

        IF wa_saida-media IS NOT INITIAL.
**  Validar cor do registro de acordo com a média de consumo
          IF wa_001-variacao IS NOT INITIAL.
            REFRESH it_color.
**  Calculo percentual de tolerância
            TRANSLATE wa_001-consumo USING ',.'.
            CONDENSE wa_001-consumo NO-GAPS.
            MOVE wa_001-consumo TO lv_consu.

            IF wa_saida-mrngu = 'KM'.
              lv_valor = lv_consu - ( ( wa_001-variacao / 100 ) + 1 ).
              IF wa_saida-media <= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
              wa_saida-cell_color[] = it_color[].
            ELSEIF wa_saida-mrngu = 'H'.
              lv_valor = lv_consu + ( ( wa_001-variacao / 100 ) + 1 ).
              IF wa_saida-media >= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
            ENDIF.
            wa_saida-cell_color[] = it_color[].
          ENDIF.

          APPEND: wa_saida TO t_saida.
        ENDIF.
        CLEAR:  wa_saida, wa_imrg_comb, wa_imrg_cont, wa_impt_comb, wa_impt_cont.

      ELSEIF p_segreg EQ abap_true.
**   Gera relatório CONSOLIDADO
        wa_saida-datab = wa_v_equi-datab.
        wa_saida-datbi = wa_v_equi-datbi.
        wa_saida-timbi = wa_v_equi-timbi.

        t_imrg_comb_aux = t_imrg_comb.
        t_imrg_cont_aux = t_imrg_cont.

        DELETE t_imrg_comb_aux
          WHERE ( idate GT wa_v_equi-datbi OR idate LT wa_v_equi-datab )
          OR ( idate EQ wa_v_equi-datab AND itime LE wa_v_equi-timei )
          OR ( idate EQ wa_v_equi-datbi AND itime GT wa_v_equi-timbi ).

        DELETE t_imrg_cont_aux
          WHERE ( idate GT wa_v_equi-datbi OR idate LT wa_v_equi-datab )
          OR ( idate EQ wa_v_equi-datab AND itime LE wa_v_equi-timei )
          OR ( idate EQ wa_v_equi-datbi AND itime GT wa_v_equi-timbi ).


        LOOP AT t_imrg_comb_aux INTO wa_imrg_comb WHERE point = wa_impt_comb-point.
          IF wa_imrg_comb-recdu IS INITIAL.
            CONTINUE.
          ENDIF.

**  Posição inicial do equipamento no período
          IF wa_saida-dt_ini IS INITIAL.
            READ TABLE t_imrg_cont_aux INTO wa_imrg_cont WITH KEY idate = wa_imrg_comb-idate
                                                              itime = wa_imrg_comb-itime.
*
            IF sy-subrc IS INITIAL.
              READ TABLE t_impt_cont INTO wa_impt_cont WITH KEY point = wa_imrg_cont-point.
              IF sy-subrc IS INITIAL.
                wa_saida-dt_ini = wa_imrg_comb-idate.
                wa_saida-t_ini  = wa_imrg_comb-itime.

                CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                  EXPORTING
                    char_unit       = wa_imrg_cont-recdu
                    decimals        = 0
                    exponent        = 0
                    fltp_value_si   = wa_imrg_cont-readg
*                   FLTP_VALUE_SI   = WA_IMRG_CONT-CDIFF
                    indicator_value = 'X'
                    masc_symbol     = ' '
                  IMPORTING
                    char_value      = lv_valor.

                wa_saida-pos_ini = lv_valor.
              ENDIF.
            ENDIF.
          ENDIF.

          IF wa_imrg_comb-idate GE s_docdat-low.
            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
              EXPORTING
                char_unit       = wa_imrg_comb-recdu
                decimals        = 0
                exponent        = 0
                fltp_value_si   = wa_imrg_comb-cdiff
                indicator_value = 'X'
                masc_symbol     = ' '
              IMPORTING
                char_value      = lv_valor.

            ADD lv_valor TO wa_saida-cdiff.
          ENDIF.                                            "Campo Consumo

***  Campo total
          READ TABLE t_impt_cont INTO wa_impt_cont WITH KEY mpobj = wa_v_equi-objnr.

          READ TABLE t_imrg_cont_aux INTO wa_imrg_cont WITH KEY idate = wa_imrg_comb-idate
                                                                itime = wa_imrg_comb-itime
                                                                point = wa_imrg_cont-point.
*          IF SY-SUBRC IS INITIAL.
*            CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
*              EXPORTING
*                CHAR_UNIT       = WA_IMRG_CONT-RECDU
*                DECIMALS        = 0
*                EXPONENT        = 0
*                FLTP_VALUE_SI   = WA_IMRG_CONT-CDIFF
*                INDICATOR_VALUE = 'X'
*                MASC_SYMBOL     = ' '
*              IMPORTING
*                CHAR_VALUE      = LV_VALOR.
*
*            ADD LV_VALOR TO WA_SAIDA-TOTAL.                                             "Campo total
*
*          ELSE.
          IF sy-subrc IS NOT INITIAL.
            CONCATENATE wa_imrg_comb-idate+6(2) wa_imrg_comb-idate+4(2) wa_imrg_comb-idate+0(4) INTO lv_data SEPARATED BY '.'.
            CONCATENATE wa_imrg_comb-itime+0(2) wa_imrg_comb-itime+2(2) wa_imrg_comb-itime+4(2) INTO lv_hora SEPARATED BY ':'.
            CONCATENATE 'Erro com o equipamento: ' wa_saida-equnr ', abastecimento para o dia '
                         lv_data ' às ' lv_hora
                        'não possui Doc. de medição. É necessário corrigir para gerar o relatório consolidado.'
            INTO lv_msg SEPARATED BY space.

            IF wg_erro IS INITIAL.
*              MESSAGE LV_MSG TYPE 'I'.
              wa_incons-l_msg = lv_msg.
              MOVE lv_data TO wa_incons-idate.
              MOVE lv_hora TO wa_incons-itime.
              MOVE wa_v_equi-swerk TO wa_incons-iwerk2.
              MOVE-CORRESPONDING wa_saida TO wa_incons.
              APPEND wa_incons TO t_incons.
*              WG_ERRO = 'X'.
            ENDIF.

          ENDIF.
        ENDLOOP.

***  Posição final para equipamento no período
        CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
          EXPORTING
            char_unit       = wa_imrg_cont-recdu
            decimals        = 0
            exponent        = 0
            fltp_value_si   = wa_imrg_cont-readg
            indicator_value = 'X'
            masc_symbol     = ' '
          IMPORTING
            char_value      = lv_valor
          EXCEPTIONS
            no_unit_given   = 1
            unit_not_found  = 2
            OTHERS          = 3.
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.


*        ENDLOOP.

        wa_saida-pos_fim = lv_valor.
        wa_saida-total   = ( wa_saida-pos_fim - wa_saida-pos_ini ).
        wa_saida-dt_fim  = wa_imrg_comb-idate.
        wa_saida-t_fim   = wa_imrg_comb-itime.

        TRY.
            IF wa_impt_cont-mrngu = 'KM'.
              lv_valor = ( wa_saida-total / wa_saida-cdiff ).
              wa_saida-media = lv_valor.
            ELSEIF wa_impt_cont-mrngu = 'H'.
              lv_valor = ( wa_saida-cdiff / wa_saida-total ).
              wa_saida-media = lv_valor.
            ENDIF.
          CATCH cx_root.

        ENDTRY.

**  Tipo do contador
        wa_saida-mrngu = wa_impt_cont-mrngu.

        IF wa_saida-media IS NOT INITIAL.
**  Validar cor do registro de acordo com a média de consumo
          IF wa_001-variacao IS NOT INITIAL.
            REFRESH it_color.
**  Calculo percentual de tolerância
            TRANSLATE wa_001-consumo USING ',.'.
            CONDENSE wa_001-consumo NO-GAPS.
            MOVE wa_001-consumo TO lv_consu.

            IF wa_saida-mrngu = 'KM'.
              lv_valor = lv_consu - ( ( wa_001-variacao / 100 ) + 1 ).
              IF wa_saida-media <= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
              wa_saida-cell_color[] = it_color[].
            ELSEIF wa_saida-mrngu = 'H'.
              lv_valor = lv_consu + ( ( wa_001-variacao / 100 ) + 1 ).
              IF wa_saida-media >= lv_valor.
                CLEAR wa_color.
                MOVE 'MEDIA'    TO wa_color-fname.
                MOVE '6'        TO wa_color-color-col.
                MOVE '1'        TO wa_color-color-int.
                MOVE '1'        TO wa_color-color-inv.
                APPEND wa_color TO it_color.
              ENDIF.
            ENDIF.
            wa_saida-cell_color[] = it_color[].
          ENDIF.

          APPEND: wa_saida TO t_saida.
        ENDIF.
        CLEAR:  wa_saida, wa_imrg_comb, wa_imrg_cont, wa_impt_comb, wa_impt_cont.

*        DELETE T_SAIDA WHERE IWERK NOT IN S_IWERK.
*        DELETE T_INCONS WHERE IWERK NOT IN S_IWERK.

      ENDIF.
    ENDIF.
*    CLEAR wa_saida.
  ENDLOOP.

  IF p_analit IS NOT INITIAL.
    DELETE t_saida WHERE dt_ini LT s_docdat-low.
  ENDIF.

  SORT t_incons ASCENDING BY iwerk iwerk2 equnr categ.

ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprimir_dados .

  IF t_saida[] IS NOT INITIAL
     AND wg_erro IS INITIAL.

    DATA: wl_layout TYPE slis_layout_alv.

    PERFORM definir_eventos.
    PERFORM ordenar_alv.
    PERFORM montar_layout.

    wl_layout-colwidth_optimize = 'X'.
    wl_layout-zebra             = 'X'.
    wl_layout-coltab_fieldname  = 'CELL_COLOR'.

    IF p_analit EQ abap_true.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = v_report
*         I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND' "sem 2º click
          it_fieldcat        = estrutura[]
          is_layout          = wl_layout
          i_save             = 'A'
          it_events          = events
          is_print           = t_print
          it_sort            = t_sort
        TABLES
          t_outtab           = t_saida
        EXCEPTIONS
          program_error      = 1.
    ELSE.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_pf_status_set = 'SET_PF_STATUS'
          i_callback_user_command  = 'USER_COMMAND'
          i_callback_program       = v_report
*         I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND' "sem 2º click
          it_fieldcat              = estrutura[]
          is_layout                = wl_layout
          i_save                   = 'A'
          it_events                = events
          is_print                 = t_print
          it_sort                  = t_sort
        TABLES
          t_outtab                 = t_saida
        EXCEPTIONS
          program_error            = 1.
    ENDIF.

  ELSE.
    MESSAGE TEXT-007 TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.


  PERFORM f_carregar_eventos USING:
*                                   SLIS_EV_USER_COMMAND  'XUSER_COMMAND',
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   slis_ev_top_of_page   'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  IF p_consol IS NOT INITIAL.
    PERFORM montar_estrutura USING:
          1   ''  ''  'T_SAIDA'   'iwerk'    'Centro'           ''  ''  '04',
          2   ''  ''  'T_SAIDA'   'equnr'    'Equipamento'      ''  ''  '10',
          3   ''  ''  'T_SAIDA'   'categ'    'Categoria'        ''  ''  '18',
          4   ''  ''  'T_SAIDA'   'typbz'    'Modelo'           ''  ''  '10',
          5   ''  ''  'T_SAIDA'   'herst'    'Fabricante'       ''  ''  '20',
          6   ''  ''  'T_SAIDA'   'baujj'    'Ano'              ''  ''  '4',
          7   ''  ''  'T_SAIDA'   'cdiff'    'Consumo'          ''  ''  '10',
          8   ''  ''  'T_SAIDA'   'dt_ini'   'Data Início'      ''  ''  '12',
          9   ''  ''  'T_SAIDA'   't_ini'    'Hora Início'      ''  ''  '08',
          10  ''  ''  'T_SAIDA'   'pos_ini'  'Odo./Hor. Início' ''  ''  '10',
          11  ''  ''  'T_SAIDA'   'dt_fim'   'Data Fim'         ''  ''  '12',
          12  ''  ''  'T_SAIDA'   't_fim'    'Hora Fim'         ''  ''  '06',
          13  ''  ''  'T_SAIDA'   'pos_fim'  'Odo./Hor. Fim'    ''  ''  '18',
          14  ''  ''  'T_SAIDA'   'total'    'Total'            ''  ''  '15',
          15  ''  ''  'T_SAIDA'   'media'    'Média Real'       ''  ''  '10',
          16  ''  ''  'T_SAIDA'   'CONSUMO'  'Média Padrão'     ''  ''  '10'.
  ELSEIF p_analit IS NOT INITIAL.
    PERFORM montar_estrutura USING:
          1   ''  ''  'T_SAIDA'   'iwerk'    'Centro'           ''  ''  '04',
          2   ''  ''  'T_SAIDA'   'equnr'    'Equipamento'      ''  ''  '10',
          3   ''  ''  'T_SAIDA'   'categ'    'Categoria'        ''  ''  '18',
          4   ''  ''  'T_SAIDA'   'typbz'    'Modelo'           ''  ''  '10',
          5   ''  ''  'T_SAIDA'   'herst'    'Fabricante'       ''  ''  '20',
          6   ''  ''  'T_SAIDA'   'baujj'    'Ano'              ''  ''  '4',
          7   ''  ''  'T_SAIDA'   'cdiff'    'Consumo'          ''  ''  '10',
          8   ''  ''  'T_SAIDA'   'dt_ini'   'Data Abast.'      ''  ''  '12',
          9   ''  ''  'T_SAIDA'   't_ini'    'Hora Abast.'      ''  ''  '08',
          10  ''  ''  'T_SAIDA'   'pos_inic'  'Odo./Inicio.'        ''  ''  '15',
          10  ''  ''  'T_SAIDA'   'pos_final'  'Odo./Final.'        ''  ''  '15',
          10  ''  ''  'T_SAIDA'   'pos_ini'  'Odo./Hor.'        ''  ''  '15',
          11  ''  ''  'T_SAIDA'   'media'    'Média Real'       ''  ''  '10',
          11  ''  ''  'T_SAIDA'   'CONSUMO'  'Média padrão'     ''  ''  '10'.
  ELSEIF p_segreg IS NOT INITIAL.
    PERFORM montar_estrutura USING:
          1   ''  ''  'T_SAIDA'   'datab'    'Dt Ini'           ''  ''  '04',
          2   ''  ''  'T_SAIDA'   'datbi'    'Dt Fim'           ''  ''  '04',
          3   ''  ''  'T_SAIDA'   'timbi'    'Hr Fim'           ''  ''  '04',
          4   ''  ''  'T_SAIDA'   'iwerk'    'Centro'           ''  ''  '04',
          5   ''  ''  'T_SAIDA'   'equnr'    'Equipamento'      ''  ''  '10',
          6   ''  ''  'T_SAIDA'   'categ'    'Categoria'        ''  ''  '18',
          7   ''  ''  'T_SAIDA'   'typbz'    'Modelo'           ''  ''  '10',
          8   ''  ''  'T_SAIDA'   'herst'    'Fabricante'       ''  ''  '20',
          9   ''  ''  'T_SAIDA'   'baujj'    'Ano'              ''  ''  '4',
          10  ''  ''  'T_SAIDA'   'cdiff'    'Consumo'          ''  ''  '10',
          11  ''  ''  'T_SAIDA'   'dt_ini'   'Data Início'      ''  ''  '12',
          12  ''  ''  'T_SAIDA'   't_ini'    'Hora Início'      ''  ''  '08',
          13  ''  ''  'T_SAIDA'   'pos_ini'  'Odo./Hor. Início' ''  ''  '10',
          14  ''  ''  'T_SAIDA'   'dt_fim'   'Data Fim'         ''  ''  '12',
          15  ''  ''  'T_SAIDA'   't_fim'    'Hora Fim'         ''  ''  '06',
          16  ''  ''  'T_SAIDA'   'pos_fim'  'Odo./Hor. Fim'    ''  ''  '18',
          17  ''  ''  'T_SAIDA'   'total'    'Total'            ''  ''  '15',
          18  ''  ''  'T_SAIDA'   'media'    'Média Real'       ''  ''  '10',
          19  ''  ''  'T_SAIDA'   'CONSUMO'  'Média Padrão'     ''  ''  '10'.
  ENDIF.
ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ORDENAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ordenar_alv.

  DATA lv_pos TYPE i.

  CLEAR lv_pos.
  REFRESH t_sort.

  IF p_segreg EQ abap_true.
    CLEAR wa_sort.
    ADD 1 TO lv_pos.
    wa_sort-spos      = lv_pos.
    wa_sort-fieldname = 'DATAB'.
    wa_sort-tabname   = 'T_SAIDA'.
    wa_sort-up        = 'X'.
*   WA_SORTDOWN.
    wa_sort-subtot    = ' '.
    APPEND wa_sort TO t_sort.

    CLEAR wa_sort.
    ADD 1 TO lv_pos.
    wa_sort-spos      = lv_pos.
    wa_sort-fieldname = 'DATBI'.
    wa_sort-tabname   = 'T_SAIDA'.
    wa_sort-up        = 'X'.
*   WA_SORTDOWN.
    wa_sort-subtot    = ' '.
    APPEND wa_sort TO t_sort.

    CLEAR wa_sort.
    ADD 1 TO lv_pos.
    wa_sort-spos      = lv_pos.
    wa_sort-fieldname = 'TIMBI'.
    wa_sort-tabname   = 'T_SAIDA'.
    wa_sort-up        = 'X'.
*   WA_SORTDOWN.
    wa_sort-subtot    = ' '.
    APPEND wa_sort TO t_sort.
  ENDIF.

  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'IWERK'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.

  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'EQUNR'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.

  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'CATEG'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.

  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'TYPBZ'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.
*
  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'HERST'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.
*
  CLEAR wa_sort.
  ADD 1 TO lv_pos.
  wa_sort-spos      = lv_pos.
  wa_sort-fieldname = 'BAUJJ'.
  wa_sort-tabname   = 'T_SAIDA'.
  wa_sort-up        = 'X'.
*   WA_SORTDOWN.
  wa_sort-subtot    = ' '.
  APPEND wa_sort TO t_sort.
*
  IF p_consol IS NOT INITIAL.
    CLEAR wa_sort.
    ADD 1 TO lv_pos.
    wa_sort-spos      = lv_pos.
    wa_sort-fieldname = 'DT_INI'.
    wa_sort-tabname   = 'T_SAIDA'.
    wa_sort-up        = 'X'.
*   WA_SORTDOWN.
    wa_sort-subtot    = ' '.
    APPEND wa_sort TO t_sort.

    CLEAR wa_sort.
    ADD 1 TO lv_pos.
    wa_sort-spos      = lv_pos.
    wa_sort-fieldname = 'T_INI'.
    wa_sort-tabname   = 'T_SAIDA'.
    wa_sort-up        = 'X'.
*   WA_SORTDOWN.
    wa_sort-subtot    = ' '.
    APPEND wa_sort TO t_sort.
  ENDIF.
ENDFORM.                    "ORDENAR
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_intlen).

  CLEAR wa_estrutura.

  wa_estrutura-edit          = p_edit.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-intlen        = p_intlen.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaveis.

  v_report = sy-repid.

  IF p_analit IS NOT INITIAL.
    PERFORM f_construir_cabecalho USING 'H' TEXT-004.
  ELSEIF p_consol IS NOT INITIAL.
    PERFORM f_construir_cabecalho USING 'H' TEXT-005.
  ENDIF.
ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  BUSCA_MODELO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_modelo .
  DATA: BEGIN OF tl_temp OCCURS 0,
          typbz TYPE zpmr0001-typbz,
        END OF tl_temp.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  REFRESH: tl_temp.

  SELECT DISTINCT typbz
    INTO TABLE tl_temp
    FROM zpmr0001.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TYPBZ'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'ZPMR0001-TYPBZ'
        value_org       = 'S'
      TABLES
        value_tab       = tl_temp
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

  ENDIF.
ENDFORM.                    " BUSCA_MODELO


*&---------------------------------------------------------------------*
*&      Form  F_MEDICAO_ANTERIOR_COMB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_POINT    text
*      -->P_DATA     text
*      -->P_HORA     text
*      -->P_MDOCM    text
*      -->C_IMRG     text
*----------------------------------------------------------------------*
FORM f_medicao_anterior_comb USING p_point TYPE imptt-point
                              p_data  TYPE imrg-idate
                              p_hora  TYPE imrg-itime
                              p_mdocm TYPE imrg-mdocm
                     CHANGING c_imrg  TYPE imrg.

  SELECT * "SINGLE *
    FROM imrg
    INTO c_imrg
    UP TO 1 ROWS
     WHERE idate EQ p_data
     AND   itime EQ p_hora
     AND  cancl EQ space
     AND  point EQ p_point
     AND  mdocm NE p_mdocm
    ORDER BY idate DESCENDING itime DESCENDING.
  ENDSELECT.

ENDFORM.                    "F_MEDICAO_ANTERIOR_COMB

*&---------------------------------------------------------------------*
*&      Form  F_MEDICAO_ANTERIOR_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_POINT    text
*      -->P_DATA     text
*      -->P_HORA     text
*      -->P_MDOCM    text
*      -->C_IMRG     text
*----------------------------------------------------------------------*
FORM f_medicao_anterior_cont USING p_point TYPE imptt-point
                              p_data  TYPE imrg-idate
                              p_hora  TYPE imrg-itime
                              p_mdocm TYPE imrg-mdocm
                     CHANGING c_imrg  TYPE imrg.

  SELECT SINGLE *
    FROM imrg
    INTO c_imrg
    WHERE idate EQ p_data
     AND  itime EQ p_hora
     AND  cancl EQ space
     AND  point EQ p_point
     AND  mdocm NE p_mdocm.

ENDFORM.                    "F_MEDICAO_ANTERIOR_CONT

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA: wl_layout_incons          TYPE slis_layout_alv.

  IF r_ucomm EQ '&INCONS'.

    PERFORM montar_layout_incons.

    wl_layout_incons-colwidth_optimize = 'X'.
    wl_layout_incons-zebra             = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title          = 'Inconsistências'
        it_fieldcat           = estrutura_incons[]
        is_layout             = wl_layout_incons
        i_screen_start_column = 20
        i_screen_start_line   = 6
        i_screen_end_column   = 110
        i_screen_end_line     = 26
      TABLES
        t_outtab              = t_incons.

  ENDIF.
ENDFORM.  "User_command

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_INCONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_incons.

  CLEAR: estrutura_incons.

  PERFORM montar_estrutura_incons USING:
      1   ''  ''  'T_INCONS'   'iwerk'    'Centro'           ''  ''  '04',
      2   ''  ''  'T_INCONS'   'iwerk2'   'Centro Resp'      ''  ''  '04',
      3   ''  ''  'T_INCONS'   'equnr'    'Equipamento'      ''  ''  '10',
      4   ''  ''  'T_INCONS'   'categ'    'Categoria'        ''  ''  '18',
      5   ''  ''  'T_INCONS'   'typbz'    'Modelo'           ''  ''  '10',
      6   ''  ''  'T_INCONS'   'herst'    'Fabricante'       ''  ''  '20',
      7   ''  ''  'T_INCONS'   'baujj'    'Ano'              ''  ''  '04',
      8   ''  ''  'T_INCONS'   'idate'    'Dt Incons.'       ''  ''  '04',
      9   ''  ''  'T_INCONS'   'itime'    'Hr Incons.'       ''  ''  '04'.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM montar_estrutura_incons USING VALUE(p_col_pos)       TYPE i
                             VALUE(p_ref_tabname)         LIKE dd02d-tabname
                             VALUE(p_ref_fieldname)       LIKE dd03d-fieldname
                             VALUE(p_tabname)             LIKE dd02d-tabname
                             VALUE(p_field)               LIKE dd03d-fieldname
                             VALUE(p_scrtext_l)           LIKE dd03p-scrtext_l
                             VALUE(p_outputlen)
                             VALUE(p_edit)
                             VALUE(p_intlen).

  CLEAR: wa_estrutura_incons.

  wa_estrutura_incons-edit          = p_edit.
  wa_estrutura_incons-fieldname     = p_field.
  wa_estrutura_incons-tabname       = p_tabname.
  wa_estrutura_incons-ref_tabname   = p_ref_tabname.
  wa_estrutura_incons-ref_fieldname = p_ref_fieldname.
  wa_estrutura_incons-key           = ' '.
  wa_estrutura_incons-key_sel       = 'X'.
  wa_estrutura_incons-col_pos       = p_col_pos.
  wa_estrutura_incons-no_out        = ' '.
  wa_estrutura_incons-seltext_s     = p_scrtext_l.
  wa_estrutura_incons-seltext_m     = p_scrtext_l.
  wa_estrutura_incons-seltext_l     = p_scrtext_l.
  wa_estrutura_incons-intlen        = p_intlen.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura_incons-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura_incons-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura_incons-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura_incons-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura_incons-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura_incons TO estrutura_incons.

ENDFORM.                    " MONTAR_ESTRUTURA
