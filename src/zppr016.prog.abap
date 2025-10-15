*======================================================================*
*                                                                      *
*                                MM                                    *
*                                                                      *
*======================================================================*
* Author      : PP                                                   *
* Date        : 08/10/2019                                             *
*----------------------------------------------------------------------*
* Project     :                                                        *
* Report      : ZPPR016                                                *
* Finality    : Inventario Defensivos controlados por Lote             *
*                                                                      *
*----------------------------------------------------------------------*
* Changes History                                                      *
*----------------------------------------------------------------------*
* Date       | Author                  | Finality                      *
* 09/10/2019 | Rodrigo Fernando Titoto |                               *
*======================================================================*
REPORT zppr016.

*----------------------------------------------------------------------*
* Tabelas                                                             *
*----------------------------------------------------------------------*
TABLES: mara, mchb, iseg, ikpf.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_log,
         matnr    TYPE mard-matnr,
         werks    TYPE mard-werks,
         lgort    TYPE mard-lgort,
         charg    TYPE mchb-charg,
         clabs    TYPE mchb-clabs,
         tipo     TYPE bapiret2-type,
         mensagem TYPE bapiret2-message,
         iblnr    TYPE iblnr,
       END OF ty_log,

       BEGIN OF ty_msg,
         type       TYPE bapi_mtype,
         id         TYPE  symsgid,
         number     TYPE  symsgno,
         message    TYPE  bapi_msg,
         log_no     TYPE  balognr,
         log_msg_no TYPE  balmnr,
         message_v1 TYPE  symsgv,
         message_v2 TYPE  symsgv,
         iblnr      TYPE iblnr,
       END OF ty_msg,

       BEGIN OF ty_desa,
         iblnr    TYPE iseg-iblnr,
         gjahr    TYPE iseg-gjahr,
         zeili    TYPE iseg-zeili,
         invnu    TYPE ikpf-invnu,
         matnr    TYPE iseg-matnr,
         werks    TYPE iseg-werks,
         lgort    TYPE iseg-lgort,
         charg    TYPE iseg-charg,
         sobkz    TYPE iseg-sobkz,
         tipo     TYPE bapiret2-type,
         mensagem TYPE bapiret2-message,
         doc_mat  TYPE mblnr,
       END OF ty_desa,

       BEGIN OF ty_dif,
         iblnr   TYPE iseg-iblnr,
         gjahr   TYPE iseg-gjahr,
         matnr   TYPE iseg-matnr,
         werks   TYPE iseg-werks,
         lgort   TYPE iseg-lgort,
         charg   TYPE iseg-charg,
         t_clabs TYPE mchb-clabs,
         d_clabs TYPE mchb-clabs,
       END OF ty_dif,

       BEGIN OF ty_transf,
         iblnr   TYPE iseg-iblnr,
         gjahr   TYPE iseg-gjahr,
         matnr   TYPE iseg-matnr,
         werks   TYPE iseg-werks,
         lgort   TYPE iseg-lgort,
         charg   TYPE iseg-charg,
         w_charg TYPE iseg-charg,
         qtd     TYPE p,
         clabs   TYPE bstmg,
         status  TYPE char255,
       END OF ty_transf,

       BEGIN OF ty_itens,
         matnr  TYPE matnr,
         charg  TYPE charg_d,
         zlicha TYPE zde_lote_forn,
         clabs  TYPE labst,
         vfdat  TYPE vfdat,
         dtval  TYPE vfdat,
         chargd TYPE charg_d,
         style  TYPE lvc_t_styl,
       END OF ty_itens.





*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: t_log         TYPE STANDARD TABLE OF ty_log,
      t_log_err     TYPE STANDARD TABLE OF ty_log,
      t_msg         TYPE STANDARD TABLE OF ty_msg,
      tl_return     TYPE STANDARD TABLE OF bapiret2,
      tl_item       TYPE TABLE OF bapi2017_gm_serialnumber,
      tl_item_goods TYPE STANDARD TABLE OF bapi2017_gm_item_create,
      wl_header     TYPE bapi2017_gm_head_01,
      tl_zppt0022   TYPE STANDARD TABLE OF zppt0022,
      t_desa        TYPE STANDARD TABLE OF ty_desa,
      t_desa_err    TYPE STANDARD TABLE OF ty_desa,
      t_desa_canc   TYPE STANDARD TABLE OF ty_desa,
      t_quant       TYPE STANDARD TABLE OF ty_log,
      t_dif         TYPE STANDARD TABLE OF ty_dif,
      tg_itens      TYPE  TABLE OF ty_transf,
      t_erro        TYPE  TABLE OF ty_transf,
      wa_tela       TYPE ty_transf,
      t_transf      TYPE STANDARD TABLE OF ty_transf,
      w_matnr       TYPE matnr.

DATA: check_canc TYPE char01.




**Declaração estutura ALV.
DATA: obj_alv_0110    TYPE REF TO cl_gui_alv_grid,
      obj_custom_0110 TYPE REF TO cl_gui_custom_container,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      gt_exc_button   TYPE ui_functions,
      it_sort         TYPE lvc_t_sort,
      it_fcat         TYPE TABLE OF lvc_s_fcat.


DATA docking        TYPE REF TO cl_gui_docking_container.
DATA splitter       TYPE REF TO cl_gui_splitter_container.
DATA custom_grid    TYPE REF TO cl_gui_custom_container.
DATA grid           TYPE REF TO cl_gui_alv_grid.
DATA alv_tree       TYPE REF TO cl_gui_alv_tree.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: v_nao_ok TYPE flag.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_matnr FOR mchb-matnr MODIF ID zg1,
                  p_matnr FOR mchb-matnr MODIF ID zg3 NO-EXTENSION NO INTERVALS.
  PARAMETERS: p_werks TYPE mchb-werks MODIF ID zg1,
              s_werks TYPE mchb-werks MODIF ID zg3,
              s_lgort TYPE mchb-lgort MODIF ID zg3,
              p_lgort TYPE mchb-lgort MODIF ID zg1.
  SELECT-OPTIONS: s_charg FOR mchb-charg MODIF ID zg1,
                  p_charg FOR mchb-charg MODIF ID zg3 NO-EXTENSION NO INTERVALS.

  PARAMETERS: p_iblnr TYPE ikpf-iblnr MODIF ID zg2,
              p_gjahr TYPE iseg-gjahr MODIF ID zg2 DEFAULT sy-datum(4).
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE .

    SELECTION-SCREEN COMMENT 3(07) TEXT-012.
*SELECTION-SCREEN POSITION 09.
    PARAMETERS: rb_agru RADIOBUTTON GROUP g1 USER-COMMAND xxx DEFAULT 'X'.

*SELECTION-SCREEN POSITION 10.
    SELECTION-SCREEN COMMENT 20(10) TEXT-013.
    PARAMETERS: rb_desa RADIOBUTTON GROUP g1.

*SELECTION-SCREEN POSITION 50.
    SELECTION-SCREEN COMMENT 40(30) TEXT-014.
    PARAMETERS: rb_ajst RADIOBUTTON GROUP g1.

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.


AT SELECTION-SCREEN OUTPUT.

  PERFORM zf_output.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
    LOOP AT er_data_changed->mt_good_cells INTO DATA(wl_good_cells) WHERE fieldname = 'VFDAT'.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_events_handler DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_events_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
ENDCLASS.                    "lcl_events_handler DEFINITION


*---------------------------------------------------------------------*
*       CLASS lcl_events_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_events_handler IMPLEMENTATION.
*---Metodo para double_click.

  METHOD handle_double_click.

    CHECK e_row-rowtype(1) EQ space.
*    PERFORM SEL_LISTA  USING E_ROW E_COLUMN-FIELDNAME.

  ENDMETHOD. " HANDLE_DOUBLE_CLICK
ENDCLASS.                    "lcl_events_handler IMPLEMENTATION

***============================================================================
*Class selec_dados_material.

CLASS zc_selec_dados DEFINITION.
  PUBLIC SECTION.

    TYPES: gt_zppet013 TYPE TABLE OF zppet013 WITH EMPTY KEY,
           gt_zppet014 TYPE TABLE OF zppet014 WITH EMPTY KEY,
           gt_zppt0011 TYPE TABLE OF zppt0011 WITH EMPTY KEY,
           gt_iseg     TYPE TABLE OF iseg WITH EMPTY KEY.


***    Methodo: Separar material que é controlado por lote e verificar o saldo.
    CLASS-METHODS: check_material EXPORTING t_zppet013 TYPE gt_zppet013  " Tabela de retorno material controlado por lote
                                            t_zppet014 TYPE gt_zppet014.  " Tabela de retorno material não controlado por lote


***    Methodo: Consulta saldo do material controlado por lote. table MCHB
    CLASS-METHODS: check_saldo_material IMPORTING i_matnr TYPE mchb-matnr  " Material
                                                  i_werks TYPE mchb-werks
                                                  i_lgort TYPE mchb-lgort
                                                  i_charg TYPE mchb-charg
                                        EXPORTING e_mchb  TYPE mchb.  " Saldo material.


***    Methodo: Consulta se o inventario informado é valido. table IKPF
    CLASS-METHODS: check_inventario IMPORTING i_iblnr TYPE iseg-iblnr  " Numero do inventario
                                              i_gjahr TYPE iseg-gjahr  " Ano
                                    EXPORTING w_ikpf  TYPE ikpf  " Retorno .
                                              return  TYPE bapiret2-message.

***    Methodo: Verificar se o inventario foi aprovado.
    CLASS-METHODS: check_status_inventario IMPORTING i_iblnr TYPE iseg-iblnr  " Numero do inventario
                                                     i_gjahr TYPE iseg-gjahr.  " Ano



***    Methodo: Verificar se o inventario foi aprovado.
    CLASS-METHODS: exib_saldo.

*** Methodo: Verificar material se existe MCHB.
    CLASS-METHODS: check_cadastro_material.


*** Methodo: Executa transferencia.
    CLASS-METHODS: set_material.


*** Methodo: Executa transferencia.
    CLASS-METHODS: set_material_zppt0011 IMPORTING i_matnr    TYPE mchb-matnr
                                                   i_werks    TYPE mchb-werks
                                                   i_lgort    TYPE mchb-lgort
                                                   i_charg    TYPE mchb-charg
                                         EXPORTING t_zppt0011 TYPE gt_zppt0011.


    CLASS-METHODS: get_fieldcatalog
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.


ENDCLASS.

CLASS zc_selec_dados IMPLEMENTATION.

  METHOD check_material.

    DATA: it_zppet013 TYPE TABLE OF zppet013.
    DATA: t_mchb TYPE TABLE OF zppet013.
    DATA: tl_zppet013 TYPE TABLE OF zppet013.
    DATA: wl_log TYPE ty_log.
    DATA: r_matnr TYPE RANGE OF zppt0011-matnr.
    DATA: w_mard TYPE mard.

    FREE: t_zppet013, t_zppet014.

***    Separar materiais controlado por lote.
    SELECT *
    FROM mara
    INTO TABLE @DATA(t_mara)
      WHERE matnr IN @s_matnr.

    CHECK t_mara IS NOT INITIAL.
    SORT t_mara BY matnr.

***Separando materiais controlado por lote.
    LOOP AT t_mara INTO DATA(w_mara).
      IF w_mara-matnr IS NOT INITIAL AND w_mara-xchpf IS NOT INITIAL.
***     Estrura recebendo materiais que é controlado por lote.
        APPEND VALUE #( matnr = w_mara-matnr ) TO t_zppet013.

      ELSE.
***     Estrura recebendo materiais que não é controlado por lote.
        APPEND VALUE #( matnr = w_mara-matnr ) TO t_zppet014.
      ENDIF.
    ENDLOOP.

****Verificar saldo dos materiais controlado por lote
    IF t_zppet013 IS NOT INITIAL.

      FREE: it_zppet013.
      SELECT *
      FROM mchb
      INTO CORRESPONDING FIELDS OF TABLE it_zppet013
        FOR ALL ENTRIES IN t_zppet013
        WHERE matnr EQ t_zppet013-matnr
          AND werks EQ p_werks
          AND lgort EQ p_lgort
*          AND CLABS NE SPACE
          AND charg IN s_charg.


      SORT it_zppet013 ASCENDING BY charg.
      LOOP AT it_zppet013 ASSIGNING FIELD-SYMBOL(<ws_zppet013>).
**  Verificar se o material tem lote INVENTARIO cadastrado.
        SELECT SINGLE *
        FROM mchb
        INTO @DATA(ws_mchb)
          WHERE matnr EQ @<ws_zppet013>-matnr
          AND  werks  EQ @<ws_zppet013>-werks
          AND  lgort  EQ @<ws_zppet013>-lgort
          AND  charg  EQ 'INVENTARIO'.
        IF sy-subrc NE 0.

          APPEND VALUE #( matnr = <ws_zppet013>-matnr
                          werks = <ws_zppet013>-werks
                          lgort = <ws_zppet013>-lgort
                          charg = <ws_zppet013>-charg
                          clabs = <ws_zppet013>-clabs
                          tipo = 'E'
                          mensagem = TEXT-011
                                      ) TO t_log_err.
          .
          CLEAR: ws_mchb.
          CONTINUE.
        ELSE.
*          IF WS_MCHB-SPERC NE SPACE.
*            APPEND VALUE #( MATNR = <WS_ZPPET013>-MATNR
*                            WERKS = <WS_ZPPET013>-WERKS
*                            LGORT = <WS_ZPPET013>-LGORT
*                            CHARG = <WS_ZPPET013>-CHARG
*                            CLABS = <WS_ZPPET013>-CLABS
*                            TIPO = 'E'
*                            MENSAGEM = TEXT-009
*                              ) TO T_LOG_ERR.
*            CONTINUE.
*          ENDIF.
        ENDIF.


        CLEAR:ws_mchb.
        SELECT SINGLE *
        FROM mchb
        INTO ws_mchb
        WHERE matnr EQ <ws_zppet013>-matnr
        AND  werks  EQ <ws_zppet013>-werks
        AND  lgort  EQ <ws_zppet013>-lgort
        AND  charg  EQ 'INVENTARIO'
        AND  sperc  EQ space.

        IF ws_mchb IS INITIAL.
          SELECT SINGLE *
          FROM zppt0011
          INTO @DATA(ws_zppt0011)
            WHERE matnr EQ  @<ws_zppet013>-matnr
             AND  werks EQ  @<ws_zppet013>-werks
             AND  lgort EQ  @<ws_zppet013>-lgort
             AND  mark  EQ  @abap_true.

          APPEND VALUE #( matnr = <ws_zppet013>-matnr
                          werks = <ws_zppet013>-werks
                          lgort = <ws_zppet013>-lgort
                          charg = <ws_zppet013>-charg
                          clabs = <ws_zppet013>-clabs
                          iblnr = ws_zppt0011-iblnr
                          tipo = 'E'
                          mensagem = TEXT-009
                           ) TO t_log_err.

          CLEAR: ws_zppt0011.
          CONTINUE.
        ENDIF.

      ENDLOOP.
      DELETE it_zppet013 WHERE charg EQ 'INVENTARIO'.
      DELETE it_zppet013 WHERE clabs EQ space.
      FREE t_zppet013.
      APPEND LINES OF it_zppet013 TO t_zppet013.
    ENDIF.

****Verificar saldo dos materiais não controlado por lote
    IF t_zppet014 IS NOT INITIAL.
      LOOP AT t_zppet014 ASSIGNING FIELD-SYMBOL(<w_zppet014>).
        IF <w_zppet014>-matnr IS NOT INITIAL.

          CLEAR:w_mard.
          SELECT SINGLE *
          FROM mard
          INTO w_mard
            WHERE matnr EQ <w_zppet014>-matnr
              AND werks EQ p_werks
              AND lgort EQ p_lgort.
*              AND LABST NE @SPACE.

          IF w_mard-labst EQ space.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = w_mard-matnr ) TO r_matnr.
          ELSE.
            <w_zppet014>-clabs =  w_mard-labst.
            <w_zppet014>-werks =  w_mard-werks.
            <w_zppet014>-lgort =  w_mard-lgort.
          ENDIF.

**** Verificar se o material esta bloqueado para inventario.
          SELECT  iblnr, gjahr, zeili, matnr, werks, lgort, charg, xdiff, xnzae, xloek
          FROM iseg
          INTO TABLE @DATA(tl_iseg)
          WHERE matnr = @<w_zppet014>-matnr
           AND  werks = @p_werks
*           AND  XDIFF EQ @SPACE
*           AND  XNZAE EQ @SPACE
           AND  lgort = @p_lgort.





          IF tl_iseg IS NOT INITIAL.
            LOOP AT tl_iseg ASSIGNING FIELD-SYMBOL(<w_iseg>) WHERE matnr EQ <w_zppet014>-matnr
                                                               AND werks EQ p_werks
                                                               AND lgort EQ p_lgort.


              IF <w_iseg>-xloek IS INITIAL.
                IF <w_iseg>-xdiff IS INITIAL AND <w_iseg>-xnzae IS INITIAL.
                  IF w_mard-sperr NE space.
                    APPEND VALUE #( matnr = <w_zppet014>-matnr
                                      werks = <w_zppet014>-werks
                                      lgort = <w_zppet014>-lgort
                                      iblnr = <w_iseg>-iblnr
                                      tipo = 'E'
                                      mensagem = TEXT-015
                                       ) TO t_log_err.

                    CLEAR: w_mard.
                    CONTINUE.
                  ENDIF.
*                APPEND VALUE #( MATNR = <W_ZPPET014>-MATNR
*                                WERKS = <W_ZPPET014>-WERKS
*                                LGORT = <W_ZPPET014>-LGORT
*                                IBLNR = TW_ISEG-IBLNR
*                                TIPO = 'E'
*                                MENSAGEM = TEXT-009
*                                 ) TO T_LOG_ERR.
                ENDIF.
                CONTINUE.
              ENDIF.

*              <W_ZPPET014>-WERKS = P_WERKS.
*              <W_ZPPET014>-LGORT = P_LGORT.
*              <W_ZPPET014>-CLABS = W_MARD-LABST.
*              CLEAR: W_MARD.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF r_matnr IS NOT INITIAL.
        SORT t_zppet014 BY matnr.
        DELETE t_zppet014 WHERE matnr IN r_matnr.
        FREE: r_matnr.
      ENDIF.
    ENDIF.
  ENDMETHOD.

******  Methodo verifica saldo material controlado por lote tabela MCHB.
  METHOD check_saldo_material.
**  Adicionando zero a esquerda.
    DATA(w_matnr) = i_matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = i_matnr
      IMPORTING
        output       = w_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE *
    FROM mchb
    INTO CORRESPONDING FIELDS OF e_mchb
     WHERE matnr EQ w_matnr
      AND werks EQ i_werks
      AND lgort EQ i_lgort
      AND charg EQ i_charg.

    IF sy-subrc NE 0.
*      RETURN = 'X'.
    ENDIF.

  ENDMETHOD.

***Verificar se o inventario é valido.
  METHOD check_inventario.
    DATA:
      vl_materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
      vl_matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year,
      t_zppt0022          TYPE TABLE OF zppt0022.



    CLEAR: w_ikpf, return.

    SELECT SINGLE *
    FROM ikpf
    INTO w_ikpf
      WHERE iblnr EQ i_iblnr
       AND  gjahr EQ i_gjahr.

    IF sy-subrc NE 0.
      return = '4'.
***   Se não existir documento verificar se ele existe na tabela ZPMT0022 e fazer a devolução.

      SELECT *
      FROM zppt0022
      INTO TABLE @DATA(gt_zppt0022)
        WHERE iblnr EQ @i_iblnr
       AND  gjahr EQ @i_gjahr.

      IF gt_zppt0022 IS NOT INITIAL.
        SORT gt_zppt0022 BY matnr werks lgort.
        SORT t_zppt0022 BY matnr werks lgort.
        APPEND LINES OF gt_zppt0022 TO t_zppt0022.
        DELETE ADJACENT DUPLICATES FROM t_zppt0022 COMPARING matnr werks lgort.

        CLEAR: tl_item_goods.
        SELECT *
        FROM zppt0011
        INTO TABLE @DATA(gt_zppt0011)
          FOR ALL ENTRIES IN @gt_zppt0022
          WHERE iblnr EQ @gt_zppt0022-iblnr.
        SORT gt_zppt0011 BY matnr werks lgort.


        LOOP AT t_zppt0022 ASSIGNING FIELD-SYMBOL(<w_zppt0022>).
          LOOP AT gt_zppt0022 ASSIGNING FIELD-SYMBOL(<l_zppt0022>) WHERE matnr EQ <w_zppt0022>-matnr
                                                                      AND werks EQ <w_zppt0022>-werks
                                                                      AND lgort EQ  <w_zppt0022>-lgort.

            CLEAR wl_header.
            wl_header-pstng_date = sy-datum.
            wl_header-doc_date   = sy-datum.

*---> 16/06/2023 - Migração S4 - DG
            DATA(v_len) = strlen( <l_zppt0022>-matnr ).

            IF v_len > 18.
              DATA(lv_move_mat_long)    = <l_zppt0022>-matnr.
              DATA(lv_material_long)    = <l_zppt0022>-matnr.
            ELSE.
              DATA(lv_move_mat)         = <l_zppt0022>-matnr.
              DATA(lv_material)         = <l_zppt0022>-matnr.
            ENDIF.
*<--- 16/06/2023 - Migração S4 - DG

            APPEND VALUE #(
                              "Transferencia De:
                              move_type  = 'ZI4'
*---> 16/06/2023 - Migração S4 - DG
                              "MOVE_MAT   = <L_ZPPT0022>-MATNR
                              move_mat      = lv_move_mat
                              move_mat_long = lv_move_mat_long
*<--- 16/06/2023 - Migração S4 - DG

                              move_plant  = <l_zppt0022>-werks
                              move_stloc = <l_zppt0022>-lgort
                              move_batch = 'INVENTARIO'

                              "Transferencia Para:
                              plant      = <l_zppt0022>-werks
*---> 16/06/2023 - Migração S4 - DG
                              "MATERIAL   = <L_ZPPT0022>-MATNR
                              material       = lv_material
                              material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                              entry_qnt  = <l_zppt0022>-clabs
                              stge_loc   = <l_zppt0022>-lgort
                              batch      = <l_zppt0022>-charg
                             ) TO tl_item_goods.

            <l_zppt0022>-mark      = ' '.
            <l_zppt0022>-iblnr     = ' '.
            <l_zppt0022>-mblnr_inv = ' '.
            <l_zppt0022>-mjahr_inv = ' '.
          ENDLOOP.


          LOOP AT gt_zppt0011 ASSIGNING FIELD-SYMBOL(<w_zppt0011>) WHERE matnr EQ  <w_zppt0022>-matnr
                                                                     AND werks EQ <w_zppt0022>-werks
                                                                     AND lgort EQ <w_zppt0022>-lgort.
*                                                                                   CHARG = <L_ZPPT0022>-CHARG
*                                                                                  GJAHR  = <L_ZPPT0022>-GJAHR.
            IF sy-subrc EQ 0.
              <w_zppt0011>-mark      = ' '.
              <w_zppt0011>-iblnr     = ' '.
              <w_zppt0011>-mblnr_inv = ' '.
              <w_zppt0011>-mjahr_inv = ' '.
            ENDIF.
          ENDLOOP.

          IF tl_item_goods IS NOT INITIAL.
            CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                goodsmvt_header  = wl_header
                goodsmvt_code    = '06'
              IMPORTING
                materialdocument = vl_materialdocument
                matdocumentyear  = vl_matdocumentyear
              TABLES
                goodsmvt_item    = tl_item_goods
                return           = tl_return.

            FREE: tl_item_goods.

            IF tl_return IS NOT INITIAL.
              DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
            ENDIF.

            IF tl_return IS NOT INITIAL.
              LOOP AT tl_return ASSIGNING FIELD-SYMBOL(<fl_return>).
                IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

                  APPEND VALUE #( gjahr = <w_zppt0022>-gjahr
                                  iblnr = <w_zppt0022>-iblnr
                                  matnr = <w_zppt0022>-matnr
                                  werks = <w_zppt0022>-werks
                                  lgort = <w_zppt0022>-lgort
*                                  CHARG = <L_ZPPT0022>-CHARG
*                                  SOBKZ = <L_ZPPT0022>-SOBKZ
*                                  INVNU = <L_ZPPT0022>-IBLNR
                                  doc_mat = vl_materialdocument
                                  tipo = 'E'
                                  mensagem = <fl_return>-message ) TO t_desa_canc.
                ENDIF.
              ENDLOOP.
            ELSE.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              APPEND VALUE #( gjahr = <w_zppt0022>-gjahr
                              iblnr = <w_zppt0022>-iblnr
                              matnr = <w_zppt0022>-matnr
                              werks = <w_zppt0022>-werks
                              lgort = <w_zppt0022>-lgort
*                              CHARG = <L_ZPPT0022>-CHARG
*                              SOBKZ = <L_ZPPT0022>-SOBKZ
*                              INVNU = <L_ZPPT0022>-IBLNR
                              doc_mat = vl_materialdocument
                              tipo = 'S'
                              mensagem = TEXT-008

                                  ) TO t_desa_canc.

              MODIFY zppt0011 FROM TABLE gt_zppt0011.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              MODIFY zppt0022 FROM TABLE gt_zppt0022.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
            ENDIF.
          ENDIF.
          CLEAR: tl_return.
        ENDLOOP.
      ENDIF.

    ELSE.
      IF w_ikpf-invnu IS NOT INITIAL.
        w_ikpf-invnu = '0' && w_ikpf-invnu.
        CONDENSE w_ikpf-invnu.
      ENDIF.
    ENDIF.
  ENDMETHOD.


***Verificar saldo do inventario.
  METHOD check_status_inventario.

    DATA: lv_valido           TYPE flag,
          tl_saldo            TYPE zppt0011-dlabs,
          dif_cont            TYPE zppt0011-dlabs,
          dif_estoq           TYPE zppt0011-dlabs,
          total_quant         TYPE zppt0011-dlabs,
          vl_msg              TYPE bapiret2-message,
          wl_headret          TYPE  bapi2017_gm_head_ret,
          tl_return           TYPE STANDARD TABLE OF bapiret2,
          lw_desa             TYPE ty_desa,
          vl_materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
          vl_matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year,
          wl_item_goods       TYPE bapi2017_gm_item_create,
*          WL_HEADER           TYPE BAPI2017_GM_HEAD_01,
          check_apr           TYPE char04,
          tl_zppt0022         TYPE TABLE OF zppt0022,
          t_check_quant       TYPE TABLE OF zppt0022,

          w_saldo             TYPE zppt0011-dlabs.

    DATA: r_charg TYPE RANGE OF zppt0011-charg.
    FREE: t_desa_canc, t_desa, t_desa_err, t_quant.


    check_inventario(
      EXPORTING
        i_iblnr = p_iblnr
        i_gjahr = p_gjahr
      IMPORTING
        w_ikpf  = DATA(w_ikpf)
        return  = DATA(w_return)
                          ).
    CHECK t_desa_canc IS INITIAL.
    IF w_return NE 0.
      MESSAGE TEXT-006 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    CLEAR: check_apr.
    IF w_ikpf-iblnr IS NOT INITIAL AND w_ikpf-invnu IS NOT INITIAL.
      SELECT *
      FROM iseg
      INTO TABLE @DATA(t_iseg)
        WHERE iblnr EQ @i_iblnr
         AND  gjahr EQ @i_gjahr.
*         AND  XDIFF NE @SPACE.

**      Verificar os lotes que fazem parte do documento inventariado.
      SELECT *
      FROM zppt0011
      INTO TABLE @DATA(t_zppt0011)
        WHERE iblnr EQ @w_ikpf-invnu
          AND gjahr EQ @i_gjahr.
      SORT t_zppt0011 BY matnr werks lgort charg.

      SELECT *
      FROM zppt0022
      INTO TABLE @DATA(t_zppt0022)
        WHERE iblnr EQ @w_ikpf-invnu
          AND gjahr EQ @i_gjahr.
      SORT t_zppt0022 BY matnr werks lgort charg.

      LOOP AT t_iseg ASSIGNING FIELD-SYMBOL(<_iseg>).
        <_iseg>-nblnr = w_ikpf-invnu.
      ENDLOOP.


    ELSE.
      FREE: t_zppt0011, t_zppt0022, t_iseg.
      SELECT *
     FROM iseg
     INTO TABLE t_iseg
       WHERE iblnr EQ i_iblnr
        AND  gjahr EQ i_gjahr.
*        AND  XDIFF NE SPACE.

**      Verificar os lotes que fazem parte do documento inventariado.
      SELECT *
      FROM zppt0011
      INTO TABLE t_zppt0011
        WHERE iblnr EQ i_iblnr
          AND gjahr EQ i_gjahr.
      SORT t_zppt0011 BY matnr werks lgort charg.

      SELECT *
      FROM zppt0022
      INTO TABLE t_zppt0022
        WHERE iblnr EQ i_iblnr
          AND gjahr EQ i_gjahr.
      SORT t_zppt0022 BY matnr werks lgort charg.
    ENDIF.

    IF t_zppt0011 IS NOT INITIAL.
      r_charg = VALUE #( FOR ls IN t_zppt0011 ( sign = 'I' option = 'EQ' low = ls-charg ) ).
    ENDIF.

    IF t_zppt0022 IS NOT INITIAL.
      APPEND LINES OF t_zppt0022 TO tl_zppt0022.

      APPEND LINES OF t_zppt0022 TO t_check_quant.

****  Materiais que esta na tabela ZPPT0016.
      DELETE tl_zppt0022 WHERE charg IN r_charg.

      SORT t_check_quant BY matnr.
      DELETE ADJACENT DUPLICATES FROM t_check_quant COMPARING matnr werks lgort.

      LOOP AT t_check_quant ASSIGNING FIELD-SYMBOL(<s_zppt0022>).
        LOOP AT t_zppt0022 ASSIGNING FIELD-SYMBOL(<ws_zppt0022>) WHERE matnr EQ <s_zppt0022>-matnr
                                                                   AND werks EQ <s_zppt0022>-werks
                                                                   AND lgort EQ <s_zppt0022>-lgort
                                                                   AND iblnr EQ i_iblnr
                                                                   AND gjahr EQ i_gjahr.
*                                                                   AND CHARG EQ <S_ZPPT0022>-CHARG.

          ADD <ws_zppt0022>-clabs TO total_quant.
        ENDLOOP.
        <s_zppt0022>-clabs = total_quant.
        <s_zppt0022>-charg = ' '.
        APPEND VALUE #( matnr = <s_zppt0022>-matnr
                        werks = <s_zppt0022>-werks
                        lgort = <s_zppt0022>-lgort
                        charg = <s_zppt0022>-charg
                        clabs = total_quant
                         ) TO t_quant.
        CLEAR: total_quant.
      ENDLOOP.
    ENDIF.

    IF t_iseg IS NOT INITIAL.
      SORT t_iseg BY matnr werks lgort charg.
      LOOP AT t_iseg ASSIGNING FIELD-SYMBOL(<sl_log>).
        IF <sl_log>-xdiff IS INITIAL OR <sl_log>-xloek IS NOT INITIAL.

          SELECT SINGLE *
          FROM mchb
          INTO @DATA(w_mchb)
            WHERE matnr EQ @<sl_log>-matnr
             AND werks EQ @<sl_log>-werks
             AND lgort EQ @<sl_log>-lgort
             AND charg EQ @<sl_log>-charg
             AND sperc EQ @space.
          IF w_mchb IS INITIAL.

            APPEND VALUE #( gjahr = <sl_log>-gjahr
                       zeili = <sl_log>-zeili
                       matnr = <sl_log>-matnr
                       werks = <sl_log>-werks
                       lgort = <sl_log>-lgort
                       charg = <sl_log>-charg
                       sobkz = <sl_log>-sobkz
                       invnu = <sl_log>-iblnr
                        tipo = 'E'
                    mensagem = TEXT-007 ) TO t_desa.
*            EXIT.
          ENDIF.
        ENDIF.
        CLEAR: w_mchb.
      ENDLOOP.

    ELSE.

      MESSAGE TEXT-006  TYPE 'I'.
      LEAVE LIST-PROCESSING.
*      LEAVE TO SCREEN 0.
    ENDIF.

    DATA: sald_mchb TYPE mchb.

    LOOP AT t_iseg ASSIGNING FIELD-SYMBOL(<lf_iseg>).
****        Verifica o saldo do item e faz o comparativo da contagem com saldo fisico se é menor que o inventario
      IF <lf_iseg>-buchm > <lf_iseg>-erfmg AND <lf_iseg>-xdiff IS NOT INITIAL.
        FREE: tl_item_goods, tl_return.
        CLEAR: tl_saldo.

        READ TABLE t_check_quant ASSIGNING FIELD-SYMBOL(<w_quant>) WITH KEY matnr = <lf_iseg>-matnr
                                                                                werks = <lf_iseg>-werks
                                                                                lgort = <lf_iseg>-lgort.

        IF sy-subrc EQ 0.
        ELSE.
          CONTINUE.
        ENDIF.


        FREE sald_mchb.
        SELECT SINGLE *
        FROM mchb
        INTO sald_mchb
          WHERE matnr = <lf_iseg>-matnr
          AND werks EQ <lf_iseg>-werks
          AND lgort EQ <lf_iseg>-lgort
          AND charg EQ 'INVENTARIO'.

*        DATA(SALD_MCHB) = <LF_ISEG>-ERFMG.

        CLEAR: tl_saldo, dif_cont, dif_estoq.
        LOOP AT t_zppt0022 ASSIGNING FIELD-SYMBOL(<w_zppt0022>) WHERE matnr EQ <lf_iseg>-matnr
                                                                  AND werks EQ <lf_iseg>-werks
                                                                  AND lgort EQ <lf_iseg>-lgort
                                                                  AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                                  AND gjahr EQ <lf_iseg>-gjahr.






          IF sald_mchb-clabs < <w_zppt0022>-clabs.

            APPEND VALUE #( gjahr = <w_zppt0022>-gjahr
                              matnr = <w_zppt0022>-matnr
                              werks = <w_zppt0022>-werks
                              lgort = <w_zppt0022>-lgort
                              charg = <w_zppt0022>-charg
                              invnu = <w_zppt0022>-iblnr
                              doc_mat = <w_zppt0022>-mblnr_inv
                              tipo = 'E'
                              mensagem = 'Sem saldo em estoque' ) TO t_desa.

            CONTINUE.
          ELSE.
            CLEAR wl_header.
            wl_header-pstng_date = sy-datum.
            wl_header-doc_date   = sy-datum.


*            IF TL_SALDO NE SPACE.
*              DIF_CONT = <LF_ISEG>-ERFMG - TL_SALDO.
*            ENDIF.

*
*            READ TABLE T_CHECK_QUANT ASSIGNING FIELD-SYMBOL(<W_QUANT>) WITH KEY MATNR = <W_ZPPT0022>-MATNR
*                                                                                WERKS = <W_ZPPT0022>-WERKS
*                                                                                LGORT = <W_ZPPT0022>-LGORT.
*
***
*
*
*
*            IF SY-SUBRC EQ 0.

*              IF DIF_ESTOQ IS INITIAL.
*                DIF_ESTOQ = <W_QUANT>-CLABS - <LF_ISEG>-ERFMG.
*              ENDIF.


***                                          Diferença do estoque - Diferença do saldo que tem que baixar.
*              IF DIF_CONT IS NOT INITIAL AND DIF_ESTOQ >= DIF_CONT.
*              IF DIF_CONT IS NOT INITIAL AND <W_QUANT>-CLABS >= DIF_ESTOQ.
*              IF SALD_MCHB > <W_QUANT>-CLABS.
*                CONTINUE.
*              ELSE.

            IF tl_zppt0022 IS NOT INITIAL.
              LOOP AT tl_zppt0022 ASSIGNING FIELD-SYMBOL(<lw_zppt0022>) WHERE  matnr EQ <w_zppt0022>-matnr
                                                                          AND  werks EQ <w_zppt0022>-werks
                                                                          AND  lgort EQ <w_zppt0022>-lgort
                                                                          AND  charg EQ <w_zppt0022>-charg
                                                                          AND gjahr  EQ <w_zppt0022>-gjahr.


*---> 16/06/2023 - Migração S4 - DG
                DATA(v_len) = strlen( <lw_zppt0022>-matnr ).

                IF v_len > 18.
                  DATA(lv_move_mat_long)    = <lw_zppt0022>-matnr.
                  DATA(lv_material_long)    = <lw_zppt0022>-matnr.
                ELSE.
                  DATA(lv_move_mat)         = <lw_zppt0022>-matnr.
                  DATA(lv_material)         = <lw_zppt0022>-matnr.
                ENDIF.
*<--- 16/06/2023 - Migração S4 - DG


                APPEND VALUE #(
                            "Transferencia De:
                            move_type  = 'ZI4'

*---> 16/06/2023 - Migração S4 - DG
                              "MOVE_MAT   = <LW_ZPPT0022>-MATNR
                              move_mat      = lv_move_mat
                              move_mat_long = lv_move_mat_long
*<--- 16/06/2023 - Migração S4 - DG
                            move_plant  = <lw_zppt0022>-werks
                            move_stloc = <lw_zppt0022>-lgort
                            move_batch = 'INVENTARIO'

                            "Transferencia Para:
                            plant      = <lw_zppt0022>-werks

*---> 16/06/2023 - Migração S4 - DG
                              "MATERIAL   = <LW_ZPPT0022>-MATNR
                              material       = lv_material
                              material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                            entry_qnt  = <lw_zppt0022>-clabs
                            stge_loc   = <lw_zppt0022>-lgort
                            batch      = <lw_zppt0022>-charg
                           ) TO tl_item_goods.



                <w_zppt0022>-mark      = ' '.
                <w_zppt0022>-iblnr     = ' '.
                <w_zppt0022>-mblnr_inv = ' '.
                <w_zppt0022>-mjahr_inv = ' '.

*                    ADD <W_ZPPT0022>-CLABS TO TL_SALDO.
                <w_quant>-clabs =  <w_quant>-clabs - <lw_zppt0022>-clabs.
                sald_mchb-clabs = sald_mchb-clabs - <lw_zppt0022>-clabs.
              ENDLOOP.
            ENDIF.

            READ TABLE t_zppt0011 ASSIGNING FIELD-SYMBOL(<w_zppt0011>) WITH KEY matnr =  <w_zppt0022>-matnr
                                                                                werks = <w_zppt0022>-werks
                                                                                lgort = <w_zppt0022>-lgort
                                                                                charg = <w_zppt0022>-charg
                                                                               gjahr  = <w_zppt0022>-gjahr.
            IF sy-subrc EQ 0.
              <w_zppt0011>-mark      = ' '.
              <w_zppt0011>-iblnr     = ' '.
              <w_zppt0011>-mblnr_inv = ' '.
              <w_zppt0011>-mjahr_inv = ' '.

*---> 16/06/2023 - Migração S4 - DG
              CLEAR : v_len, lv_move_mat_long, lv_move_mat, lv_material_long, lv_material.

              v_len = strlen( <w_zppt0022>-matnr ).

              IF v_len > 18.
                lv_move_mat_long    = <w_zppt0022>-matnr.
                lv_material_long    = <w_zppt0022>-matnr.
              ELSE.
                lv_move_mat         = <w_zppt0022>-matnr.
                lv_material         = <w_zppt0022>-matnr.
              ENDIF.
*<--- 16/06/2023 - Migração S4 - DG

              APPEND VALUE #(
                          "Transferencia De:
                          move_type  = 'ZI4'
*---> 16/06/2023 - Migração S4 - DG
                              "MOVE_MAT   = <W_ZPPT0022>-MATNR
                              move_mat      = lv_move_mat
                              move_mat_long = lv_move_mat_long
*<--- 16/06/2023 - Migração S4 - DG
                          move_plant = <w_zppt0022>-werks
                          move_stloc = <w_zppt0022>-lgort
                          move_batch = 'INVENTARIO'

                          "Transferencia Para:
                          plant      = <w_zppt0022>-werks
*---> 16/06/2023 - Migração S4 - DG
                              "MATERIAL   = <W_ZPPT0022>-MATNR
                              material       = lv_material
                              material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                          entry_qnt  = <w_zppt0022>-clabs
                          stge_loc   = <w_zppt0022>-lgort
                          batch      = <w_zppt0022>-charg
                         ) TO tl_item_goods.

              <w_zppt0022>-mark      = ' '.
              <w_zppt0022>-iblnr     = ' '.
              <w_zppt0022>-mblnr_inv = ' '.
              <w_zppt0022>-mjahr_inv = ' '.
*              ADD <W_ZPPT0022>-CLABS TO TL_SALDO.
              <w_quant>-clabs =  <w_quant>-clabs - <w_zppt0022>-clabs.
              sald_mchb-clabs = sald_mchb-clabs - <w_zppt0022>-clabs.
*                  CLEAR: DIF_CONT.
*                ENDIF.
*              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF tl_item_goods IS NOT INITIAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = '06'
            IMPORTING
              materialdocument = vl_materialdocument
              matdocumentyear  = vl_matdocumentyear
            TABLES
              goodsmvt_item    = tl_item_goods
              return           = tl_return.
        ELSE.
          CONTINUE.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          LOOP AT tl_return ASSIGNING FIELD-SYMBOL(<fl_return>).
            IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

              APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                              zeili = <lf_iseg>-zeili
                              matnr = <lf_iseg>-matnr
                              werks = <lf_iseg>-werks
                              lgort = <lf_iseg>-lgort
                              charg = <lf_iseg>-charg
                              sobkz = <lf_iseg>-sobkz
                              invnu = <lf_iseg>-iblnr
                              doc_mat = vl_materialdocument
                              tipo = 'E'
                              mensagem = <fl_return>-message ) TO t_desa.
            ENDIF.
          ENDLOOP.

        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                          zeili = <lf_iseg>-zeili
                          matnr = <lf_iseg>-matnr
                          werks = <lf_iseg>-werks
                          lgort = <lf_iseg>-lgort
                          charg = <lf_iseg>-charg
                          sobkz = <lf_iseg>-sobkz
                          invnu = <lf_iseg>-iblnr
                          doc_mat = vl_materialdocument
                          tipo = 'S'
                          mensagem = TEXT-008

                              ) TO t_desa.

          MODIFY zppt0011 FROM TABLE t_zppt0011.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          MODIFY zppt0022 FROM TABLE t_zppt0022.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          CONTINUE.
        ENDIF.

**=========================================================================================================================
****        Verifica o saldo do item e faz o comparativo da contagem com saldo fisico se é maior que o inventario.
      ELSEIF <lf_iseg>-buchm < <lf_iseg>-erfmg AND <lf_iseg>-xdiff IS NOT INITIAL .
        FREE: tl_item_goods, tl_return.
        LOOP AT t_zppt0011 ASSIGNING <w_zppt0011> WHERE matnr EQ <lf_iseg>-matnr
                                                    AND werks EQ <lf_iseg>-werks
                                                    AND lgort EQ <lf_iseg>-lgort
*                                                    AND CHARG EQ <LF_ISEG>-CHARG
                                                    AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                    AND gjahr EQ <lf_iseg>-gjahr.


          <w_zppt0011>-mark      = ' '.
          <w_zppt0011>-iblnr     = ' '.
          <w_zppt0011>-mblnr_inv = ' '.
          <w_zppt0011>-mjahr_inv = ' '.

        ENDLOOP.

        LOOP AT t_zppt0022 ASSIGNING <w_zppt0022> WHERE matnr EQ <lf_iseg>-matnr
                                                      AND werks EQ <lf_iseg>-werks
                                                      AND lgort EQ <lf_iseg>-lgort
*                                                      AND CHARG EQ <LF_ISEG>-CHARG
                                                      AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                      AND gjahr EQ <lf_iseg>-gjahr.


          CLEAR wl_header.
          wl_header-pstng_date = sy-datum.
          wl_header-doc_date   = sy-datum.

*---> 16/06/2023 - Migração S4 - DG
          CLEAR : v_len, lv_move_mat_long, lv_move_mat, lv_material_long, lv_material.

          v_len = strlen( <w_zppt0022>-matnr ).

          IF v_len > 18.
            lv_move_mat_long    = <w_zppt0022>-matnr.
            lv_material_long    = <w_zppt0022>-matnr.
          ELSE.
            lv_move_mat         = <w_zppt0022>-matnr.
            lv_material         = <w_zppt0022>-matnr.
          ENDIF.
*<--- 16/06/2023 - Migração S4 - DG


          APPEND VALUE #(
                           "Transferencia De:
                            move_type  = 'ZI4'
*---> 16/06/2023 - Migração S4 - DG
                              "MOVE_MAT   = <W_ZPPT0022>-MATNR
                              move_mat      = lv_move_mat
                              move_mat_long = lv_move_mat_long
*<--- 16/06/2023 - Migração S4 - DG
                            move_plant = <w_zppt0022>-werks
                            move_stloc = <w_zppt0022>-lgort
                            move_batch = 'INVENTARIO'

                            "Transferencia Para:
                            plant      = <w_zppt0022>-werks
*---> 16/06/2023 - Migração S4 - DG
                              "MATERIAL   = <W_ZPPT0022>-MATNR
                              material       = lv_material
                              material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                            entry_qnt  = <w_zppt0022>-clabs
                            stge_loc   = <w_zppt0022>-lgort
                            batch      = <w_zppt0022>-charg
                           ) TO tl_item_goods.

          <w_zppt0022>-mark      = ' '.
          <w_zppt0022>-iblnr     = ' '.
          <w_zppt0022>-mblnr_inv = ' '.
          <w_zppt0022>-mjahr_inv = ' '.

        ENDLOOP.

*        IF TL_ITEM_GOODS IS INITIAL.
*          MESSAGE TEXT-004 TYPE 'I'.
*          LEAVE LIST-PROCESSING.
*        ENDIF.


        IF tl_item_goods IS NOT INITIAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = '06'
            IMPORTING
              materialdocument = vl_materialdocument
              matdocumentyear  = vl_matdocumentyear
            TABLES
              goodsmvt_item    = tl_item_goods
              return           = tl_return.
        ELSE.
          CONTINUE.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          LOOP AT tl_return ASSIGNING <fl_return>.
            IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

              APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                              zeili = <lf_iseg>-zeili
                              matnr = <lf_iseg>-matnr
                              werks = <lf_iseg>-werks
                              lgort = <lf_iseg>-lgort
                              charg = <lf_iseg>-charg
                              sobkz = <lf_iseg>-sobkz
                              invnu = <lf_iseg>-iblnr
                              doc_mat = vl_materialdocument
                              tipo = 'E'
                              mensagem = <fl_return>-message ) TO t_desa.


            ENDIF.
          ENDLOOP.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                          zeili = <lf_iseg>-zeili
                          matnr = <lf_iseg>-matnr
                          werks = <lf_iseg>-werks
                          lgort = <lf_iseg>-lgort
                          charg = <lf_iseg>-charg
                          sobkz = <lf_iseg>-sobkz
                          invnu = <lf_iseg>-iblnr
                          doc_mat = vl_materialdocument
                          tipo = 'S'
                          mensagem = TEXT-008 ) TO t_desa.


          MODIFY zppt0011 FROM TABLE t_zppt0011.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          MODIFY zppt0022 FROM TABLE t_zppt0022.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          CONTINUE.
        ENDIF.
**=========================================================================================================================
****        Verifica o saldo do item e faz o comparativo da contagem com saldo fisico se é igual o saldo do inventario.
      ELSEIF <lf_iseg>-buchm EQ <lf_iseg>-erfmg AND <lf_iseg>-xdiff IS NOT INITIAL .
        FREE: tl_item_goods, tl_return.
        LOOP AT t_zppt0011 ASSIGNING <w_zppt0011> WHERE matnr EQ <lf_iseg>-matnr
                                                    AND werks EQ <lf_iseg>-werks
                                                    AND lgort EQ <lf_iseg>-lgort
*                                                    AND CHARG EQ <LF_ISEG>-CHARG
                                                    AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                    AND gjahr EQ <lf_iseg>-gjahr.



          <w_zppt0011>-mark      = ' '.
          <w_zppt0011>-iblnr     = ' '.
          <w_zppt0011>-mblnr_inv = ' '.
          <w_zppt0011>-mjahr_inv = ' '.

        ENDLOOP.

        LOOP AT t_zppt0022 ASSIGNING <w_zppt0022> WHERE matnr EQ <lf_iseg>-matnr
                                                      AND werks EQ <lf_iseg>-werks
                                                      AND lgort EQ <lf_iseg>-lgort
*                                                      AND CHARG EQ <LF_ISEG>-CHARG
                                                      AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                      AND gjahr EQ <lf_iseg>-gjahr.



          CLEAR wl_header.
          wl_header-pstng_date = sy-datum.
          wl_header-doc_date   = sy-datum.

*---> 16/06/2023 - Migração S4 - DG
          CLEAR : v_len, lv_move_mat_long, lv_move_mat, lv_material_long, lv_material.

          v_len = strlen( <w_zppt0022>-matnr ).

          IF v_len > 18.
            lv_move_mat_long    = <w_zppt0022>-matnr.
            lv_material_long    = <w_zppt0022>-matnr.
          ELSE.
            lv_move_mat         = <w_zppt0022>-matnr.
            lv_material         = <w_zppt0022>-matnr.
          ENDIF.
*<--- 16/06/2023 - Migração S4 - DG

          APPEND VALUE #(
                           "Transferencia De:
                            move_type  = 'ZI4'
*---> 16/06/2023 - Migração S4 - DG
                              "MOVE_MAT   = <W_ZPPT0022>-MATNR
                              move_mat      = lv_move_mat
                              move_mat_long = lv_move_mat_long
*<--- 16/06/2023 - Migração S4 - DG
                            move_plant = <w_zppt0022>-werks
                            move_stloc = <w_zppt0022>-lgort
                            move_batch = 'INVENTARIO'

                            "Transferencia Para:
                            plant      = <w_zppt0022>-werks
*---> 16/06/2023 - Migração S4 - DG
                              "MATERIAL   = <W_ZPPT0022>-MATNR
                              material       = lv_material
                              material_long  = lv_material_long
*<--- 16/06/2023 - Migração S4 - DG
                            entry_qnt  = <w_zppt0022>-clabs
                            stge_loc   = <w_zppt0022>-lgort
                            batch      = <w_zppt0022>-charg
                           ) TO tl_item_goods.

          <w_zppt0022>-mark      = ' '.
          <w_zppt0022>-iblnr     = ' '.
          <w_zppt0022>-mblnr_inv = ' '.
          <w_zppt0022>-mjahr_inv = ' '.

        ENDLOOP.

*        IF TL_ITEM_GOODS IS INITIAL.
*          MESSAGE TEXT-004 TYPE 'I'.
*          LEAVE LIST-PROCESSING.
*        ENDIF.


        IF tl_item_goods IS NOT INITIAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = '06'
            IMPORTING
              materialdocument = vl_materialdocument
              matdocumentyear  = vl_matdocumentyear
            TABLES
              goodsmvt_item    = tl_item_goods
              return           = tl_return.
        ELSE.
          CONTINUE.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          LOOP AT tl_return ASSIGNING <fl_return>.
            IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

              APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                              zeili = <lf_iseg>-zeili
                              matnr = <lf_iseg>-matnr
                              werks = <lf_iseg>-werks
                              lgort = <lf_iseg>-lgort
                              charg = <lf_iseg>-charg
                              sobkz = <lf_iseg>-sobkz
                              invnu = <lf_iseg>-iblnr
                              doc_mat = vl_materialdocument
                              tipo = 'E'
                              mensagem = <fl_return>-message ) TO t_desa.


            ENDIF.
          ENDLOOP.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                          zeili = <lf_iseg>-zeili
                          matnr = <lf_iseg>-matnr
                          werks = <lf_iseg>-werks
                          lgort = <lf_iseg>-lgort
                          charg = <lf_iseg>-charg
                          sobkz = <lf_iseg>-sobkz
                          invnu = <lf_iseg>-iblnr
                          doc_mat = vl_materialdocument
                          tipo = 'S'
                          mensagem = TEXT-008 ) TO t_desa.


          MODIFY zppt0011 FROM TABLE t_zppt0011.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          MODIFY zppt0022 FROM TABLE t_zppt0022.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          CONTINUE.
        ENDIF.
**=========================================================================================================================
****        Verifica se o material foi eliminado.
      ELSEIF <lf_iseg>-xloek IS NOT INITIAL .
        FREE: tl_item_goods, tl_return.
        LOOP AT t_zppt0011 ASSIGNING <w_zppt0011> WHERE matnr EQ <lf_iseg>-matnr
                                                    AND werks EQ <lf_iseg>-werks
                                                    AND lgort EQ <lf_iseg>-lgort
*                                                    AND CHARG EQ <LF_ISEG>-CHARG
                                                    AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                    AND gjahr EQ <lf_iseg>-gjahr.


          <w_zppt0011>-mark      = ' '.
          <w_zppt0011>-iblnr     = ' '.
          <w_zppt0011>-mblnr_inv = ' '.
          <w_zppt0011>-mjahr_inv = ' '.

        ENDLOOP.

        LOOP AT t_zppt0022 ASSIGNING <w_zppt0022> WHERE matnr EQ <lf_iseg>-matnr
                                                      AND werks EQ <lf_iseg>-werks
                                                      AND lgort EQ <lf_iseg>-lgort
*                                                      AND CHARG EQ <LF_ISEG>-CHARG
                                                      AND ( iblnr EQ <lf_iseg>-iblnr OR iblnr EQ <lf_iseg>-nblnr )
                                                      AND gjahr EQ <lf_iseg>-gjahr.


          CLEAR wl_header.
          wl_header-pstng_date = sy-datum.
          wl_header-doc_date   = sy-datum.

          APPEND VALUE #(
                           "Transferencia De:
                            move_type  = 'ZI4'
                            move_mat   = <w_zppt0022>-matnr
                            move_plant = <w_zppt0022>-werks
                            move_stloc = <w_zppt0022>-lgort
                            move_batch = 'INVENTARIO'

                            "Transferencia Para:
                            plant      = <w_zppt0022>-werks
                            material   = <w_zppt0022>-matnr
                            entry_qnt  = <w_zppt0022>-clabs
                            stge_loc   = <w_zppt0022>-lgort
                            batch      = <w_zppt0022>-charg
                           ) TO tl_item_goods.

          <w_zppt0022>-mark      = ' '.
          <w_zppt0022>-iblnr     = ' '.
          <w_zppt0022>-mblnr_inv = ' '.
          <w_zppt0022>-mjahr_inv = ' '.

        ENDLOOP.

*        IF TL_ITEM_GOODS IS INITIAL.
*          MESSAGE TEXT-004 TYPE 'I'.
*          LEAVE LIST-PROCESSING.
*        ENDIF.

        IF tl_item_goods IS NOT INITIAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = '06'
            IMPORTING
              materialdocument = vl_materialdocument
              matdocumentyear  = vl_matdocumentyear
            TABLES
              goodsmvt_item    = tl_item_goods
              return           = tl_return.
        ELSE.
          CONTINUE.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
        ENDIF.

        IF tl_return IS NOT INITIAL.
          LOOP AT tl_return ASSIGNING <fl_return>.
            IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

              APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                              zeili = <lf_iseg>-zeili
                              matnr = <lf_iseg>-matnr
                              werks = <lf_iseg>-werks
                              lgort = <lf_iseg>-lgort
                              charg = <lf_iseg>-charg
                              sobkz = <lf_iseg>-sobkz
                              invnu = <lf_iseg>-iblnr
                              doc_mat = vl_materialdocument
                              tipo = 'E'
                              mensagem = <fl_return>-message ) TO t_desa.


            ENDIF.
          ENDLOOP.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          APPEND VALUE #( gjahr = <lf_iseg>-gjahr
                          zeili = <lf_iseg>-zeili
                          matnr = <lf_iseg>-matnr
                          werks = <lf_iseg>-werks
                          lgort = <lf_iseg>-lgort
                          charg = <lf_iseg>-charg
                          sobkz = <lf_iseg>-sobkz
                          invnu = <lf_iseg>-iblnr
                          doc_mat = vl_materialdocument
                          tipo = 'S'
                          mensagem = TEXT-008 ) TO t_desa.


          MODIFY zppt0011 FROM TABLE t_zppt0011.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          MODIFY zppt0022 FROM TABLE t_zppt0022.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

****Verifica o saldo do material informado para acerto apos inventario.
  METHOD exib_saldo.

    SELECT SINGLE *
    FROM mchb
    INTO CORRESPONDING FIELDS OF wa_tela
    WHERE matnr IN p_matnr
      AND werks EQ s_werks
      AND lgort EQ s_lgort
      AND charg IN p_charg
      AND clabs NE space.

    IF wa_tela IS INITIAL.
      MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      APPEND wa_tela TO t_transf.
      CALL SCREEN 0200.
    ENDIF.
  ENDMETHOD.

  METHOD check_cadastro_material.
    FREE: t_erro.
    IF  tg_itens IS NOT INITIAL.
      LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<w_itens>).
        IF <w_itens>-clabs NE space.
          SELECT SINGLE *
          FROM mchb
          INTO @DATA(w_mchb)
            WHERE matnr EQ @<w_itens>-matnr
              AND werks EQ @<w_itens>-werks
              AND lgort EQ @<w_itens>-lgort
              AND charg EQ @<w_itens>-w_charg.

          IF w_mchb IS INITIAL.
            APPEND VALUE #(  matnr     = <w_itens>-matnr
                                werks     = <w_itens>-werks
                                lgort     = <w_itens>-lgort
                                charg     = <w_itens>-charg
                                w_charg   = <w_itens>-w_charg
                                clabs     = <w_itens>-clabs
                                status    = icon_red_light && 'Lote não existe' ) TO t_erro.

          ELSE.

            SELECT SINGLE *
            FROM zppt0011
            INTO @DATA(ws_zppt0011)
              WHERE matnr EQ @<w_itens>-matnr
              AND werks EQ @<w_itens>-werks
              AND lgort EQ @<w_itens>-lgort
              AND charg EQ @<w_itens>-w_charg.
            IF ws_zppt0011 IS INITIAL.
              APPEND VALUE #(  matnr     = <w_itens>-matnr
                                  werks     = <w_itens>-werks
                                  lgort     = <w_itens>-lgort
                                  charg     = <w_itens>-charg
                                  w_charg   = <w_itens>-w_charg
                                  clabs     = <w_itens>-clabs
                                  status    = icon_red_light && 'Lote não é um item controlado' ) TO t_erro.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ELSE.

          APPEND VALUE #(  matnr     = <w_itens>-matnr
                             werks     = <w_itens>-werks
                             lgort     = <w_itens>-lgort
                             charg     = <w_itens>-charg
                             w_charg   = <w_itens>-w_charg
                             clabs     = <w_itens>-clabs
                             status    = icon_red_light && 'Informar o volume' ) TO t_erro.


        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.

  METHOD set_material.

    DATA: wl_header           TYPE bapi2017_gm_head_01,
          vl_materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
          vl_matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year.
    FREE: t_erro, tl_return, tl_item_goods.

    CLEAR wl_header.
    wl_header-pstng_date = sy-datum.
    wl_header-doc_date = sy-datum.

    SELECT *
    FROM zppt0011
    INTO TABLE @DATA(gt_zppt0011)
      FOR ALL ENTRIES IN @tg_itens
      WHERE matnr EQ @tg_itens-matnr
       AND  werks EQ @tg_itens-werks
       AND  lgort EQ @tg_itens-lgort
       AND  charg EQ @tg_itens-w_charg.

    SELECT *
  FROM zppt0022
  INTO TABLE @DATA(gt_zppt0022)
    FOR ALL ENTRIES IN @tg_itens
    WHERE matnr EQ @tg_itens-matnr
     AND  werks EQ @tg_itens-werks
     AND  lgort EQ @tg_itens-lgort
     AND  charg EQ @tg_itens-w_charg.

    IF  tg_itens IS NOT INITIAL.
      LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<w_itens>).
        IF <w_itens>-charg IS NOT INITIAL.
          LOOP AT gt_zppt0011 ASSIGNING FIELD-SYMBOL(<w_zppt0011>) WHERE  matnr EQ <w_itens>-matnr
                                                                     AND  werks EQ <w_itens>-werks
                                                                     AND  lgort EQ <w_itens>-lgort
                                                                     AND  charg EQ <w_itens>-w_charg.

            IF <w_itens>-charg EQ <w_itens>-w_charg.

              APPEND VALUE #(  matnr     = <w_itens>-matnr
                               werks     = <w_itens>-werks
                               lgort     = <w_itens>-lgort
                               charg     = <w_itens>-charg
                               w_charg   = <w_itens>-w_charg
                               clabs     = <w_itens>-clabs
                               status    = icon_red_light && 'Informar um lote diferente valido, pendente inventario' ) TO t_erro.

            ELSE.


              IF <w_zppt0011>-mark IS NOT INITIAL.
                READ TABLE gt_zppt0022 ASSIGNING FIELD-SYMBOL(<w_zppt0022>) WITH KEY matnr = <w_itens>-matnr
                                                                                    werks = <w_itens>-werks
                                                                                    lgort = <w_itens>-lgort
                                                                                    charg = <w_itens>-w_charg.

                IF sy-subrc EQ 0.
                  IF <w_itens>-clabs > <w_zppt0022>-clabs.

                    APPEND VALUE #(  matnr     = <w_itens>-matnr
                                     werks     = <w_itens>-werks
                                     lgort     = <w_itens>-lgort
                                     charg     = <w_itens>-charg
                                     w_charg   = <w_itens>-w_charg
                                     clabs     = <w_itens>-clabs
                                     status    =  icon_red_light && 'Verificar quantidade informada, diverge do fator de embalagem definida' ) TO t_erro.
                  ELSE.

                    <w_zppt0011>-mark      = ' '.
                    <w_zppt0011>-iblnr     = ' '.
                    <w_zppt0011>-mblnr_inv = ' '.
                    <w_zppt0011>-mjahr_inv = ' '.

                    <w_zppt0022>-clabs     = <w_itens>-clabs.
                    <w_zppt0022>-mark      = ' '.
                    <w_zppt0022>-iblnr     = ' '.
                    <w_zppt0022>-mblnr_inv = ' '.
                    <w_zppt0022>-mjahr_inv = ' '.


                    APPEND VALUE #( move_type  = '311'
                              plant      = <w_itens>-werks
                              material   = <w_itens>-matnr
                              entry_qnt  = <w_itens>-clabs
                              stge_loc   = <w_itens>-lgort
                              batch      = <w_itens>-charg
                              move_stloc = <w_itens>-lgort
                              move_batch = <w_itens>-w_charg
                                   ) TO tl_item_goods.
                  ENDIF.

                ELSE.


                ENDIF.


              ELSE.
                APPEND VALUE #(  matnr     = <w_itens>-matnr
                                   werks     = <w_itens>-werks
                                   lgort     = <w_itens>-lgort
                                   charg     = <w_itens>-charg
                                   w_charg   = <w_itens>-w_charg
                                   clabs     = <w_itens>-clabs
                                   status    = icon_red_light && ' Lote não não é um itens pendente de inventario'
                                   ) TO t_erro.

              ENDIF.
            ENDIF.
          ENDLOOP.

*          FREE: GT_ZPPT0011.
*          ZC_SELEC_DADOS=>SET_MATERIAL_ZPPT0011(
*            EXPORTING
*              I_MATNR    = <W_ITENS>-MATNR
*              I_WERKS    = <W_ITENS>-WERKS
*              I_LGORT    = <W_ITENS>-LGORT
*              I_CHARG    = <W_ITENS>-W_CHARG
*            IMPORTING
*              T_ZPPT0011 = GT_ZPPT0011
*          ).

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF t_erro IS NOT INITIAL.
      FREE: tg_itens.
      APPEND LINES OF t_erro TO tg_itens.
    ELSE.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = wl_header
          goodsmvt_code    = '06'
        IMPORTING
          materialdocument = vl_materialdocument
          matdocumentyear  = vl_matdocumentyear
        TABLES
          goodsmvt_item    = tl_item_goods
          return           = tl_return.

      IF tl_return IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
        READ TABLE tl_return INTO DATA(w_retur) INDEX 1.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        MESSAGE 'Erro ' && w_retur-message TYPE  'E' DISPLAY LIKE 'I'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        MODIFY zppt0011 FROM TABLE gt_zppt0011.
        COMMIT WORK.

        MODIFY zppt0022 FROM TABLE gt_zppt0022.
        COMMIT WORK.

        MESSAGE 'Documento =>' && vl_materialdocument  && ' Criado com sucesso' TYPE 'S' DISPLAY LIKE 'I'.
        FREE: gt_zppt0011, tl_return, tl_item_goods.
        LEAVE TO SCREEN 0.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_material_zppt0011.

    DATA: lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = i_matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE *
    FROM zppt0011
    INTO @DATA(ws_zppt0011)
      WHERE  matnr EQ  @lv_matnr
        AND  werks EQ  @i_werks
        AND  lgort EQ  @i_lgort
        AND  charg EQ  @i_charg.

    IF ws_zppt0011 IS NOT INITIAL.
      APPEND ws_zppt0011 TO  t_zppt0011.
    ENDIF.

  ENDMETHOD.

  METHOD get_fieldcatalog.

    fcat =
            VALUE #(
                      ( fieldname = 'MATNR'   coltext = 'Material'        outputlen = 12 no_zero = abap_true )
                      ( fieldname = 'WERKS'   coltext = 'Centro'          outputlen = 12 )
                      ( fieldname = 'LGORT'   coltext = 'Depósito'        outputlen = 30 )
                      ( fieldname = 'CHARG'   coltext = 'Lote origem'     outputlen = 10 )
                      ( fieldname = 'W_CHARG' coltext = 'Lote destino =>' outputlen = 10 edit = abap_true )
                      ( fieldname = 'CLABS'   coltext = 'Volume'          outputlen = 10 edit = abap_true    ref_field = 'CLABS'     ref_table = 'MCHB'  )
                      ( fieldname = 'STATUS'  coltext = 'Status =>       ' outputlen = 15 edit = abap_false )
                   ).

  ENDMETHOD.
ENDCLASS.

***============================================================================

*----------------------------------------------------------------------*
* Inicio da Seleção                                                    *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = s_matnr-low
    IMPORTING
      output       = s_matnr-low
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = s_matnr-high
    IMPORTING
      output       = s_matnr-high
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.



  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = p_matnr
    IMPORTING
      output       = p_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.



  PERFORM zf_limpar.

  IF NOT rb_agru IS INITIAL.
    IF s_matnr IS INITIAL OR p_werks IS INITIAL OR p_lgort IS INITIAL.
      MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*    PERFORM ZF_SELECIONAR.
    PERFORM zf_executar.
    PERFORM zf_exibir_log.

  ELSEIF rb_ajst IS NOT INITIAL.
    IF p_matnr IS INITIAL OR s_werks IS INITIAL OR s_lgort IS INITIAL OR p_charg IS INITIAL.
      MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.


    zc_selec_dados=>exib_saldo( ).

  ELSE.

    IF p_iblnr IS INITIAL OR p_gjahr IS INITIAL.
      MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*    PERFORM ZF_SELECIONAR_DESA.
    zc_selec_dados=>check_status_inventario(
      EXPORTING
        i_iblnr = p_iblnr
        i_gjahr = p_gjahr
    ).

    PERFORM zf_exibir_log.

  ENDIF.

*----------------------------------------------------------------------*
* Rotinas                                                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZF_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_output .


  IF rb_agru IS NOT INITIAL.
    CLEAR: p_iblnr, p_gjahr,  s_werks, s_lgort, p_charg[], p_matnr[].
  ENDIF.

  IF rb_desa IS NOT INITIAL.
    CLEAR:  s_matnr[], p_werks, p_lgort, s_charg[], s_werks, s_lgort, p_charg[], p_matnr[].
  ENDIF.

  IF rb_ajst IS NOT INITIAL.
    CLEAR: p_iblnr, p_gjahr, s_matnr, p_werks, p_lgort, s_charg[].
  ENDIF.

  LOOP AT SCREEN.

***DEMOSTRAR OS CAMPOS DO GRUPO 1
    IF screen-group1 EQ 'ZG2'.
      IF rb_agru IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'ZG3'.
      IF rb_agru IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

***DEMOSTRAR OS CAMPOS DO GRUPO 2
    IF screen-group1 EQ 'ZG1'.
      IF rb_desa IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'ZG3'.
      IF rb_desa IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

***DEMOSTRAR OS CAMPOS DO GRUPO 3
    IF screen-group1 EQ 'ZG1'.
      IF rb_ajst IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'ZG2'.
      IF rb_ajst IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_limpar .

  REFRESH: t_desa, t_log.

  CLEAR: v_nao_ok.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_selecionar .

  DATA: wl_log TYPE ty_log.

  SELECT *
      FROM mard
      INTO TABLE @DATA(tl_mard)
      WHERE matnr IN @s_matnr
      AND werks = @p_werks
      AND lgort = @p_lgort.
  IF sy-subrc IS INITIAL.

    SELECT *
              FROM mchb
              INTO TABLE @DATA(tl_mchb)
              FOR ALL ENTRIES IN @tl_mard
             WHERE matnr =  @tl_mard-matnr
               AND werks =  @tl_mard-werks
               AND lgort =  @tl_mard-lgort
               AND charg IN @s_charg.

    IF sy-subrc IS INITIAL.

      SORT tl_mchb BY matnr werks lgort charg.


      SELECT iblnr, gjahr, zeili, matnr, werks, lgort, charg, xdiff
             INTO TABLE @DATA(tl_iseg)
             FROM iseg
             FOR ALL ENTRIES IN @tl_mchb
             WHERE matnr = @tl_mchb-matnr AND
                   werks = @tl_mchb-werks AND
                   lgort = @tl_mchb-lgort AND
                   charg = @tl_mchb-charg AND
                   xdiff = ''.
      IF sy-subrc IS INITIAL.

        SORT tl_iseg BY matnr werks lgort charg.

      ENDIF.

    ENDIF.

  ENDIF.

  IF tl_mchb[] IS INITIAL.

    MESSAGE TEXT-004 TYPE 'I'.
    LEAVE LIST-PROCESSING.

  ENDIF.

  CLEAR v_nao_ok.
  REFRESH t_log.

  LOOP AT tl_mard ASSIGNING FIELD-SYMBOL(<fl_mard>).

    CLEAR wl_log.
    wl_log-matnr = <fl_mard>-matnr.
    wl_log-werks = <fl_mard>-werks.
    wl_log-lgort = <fl_mard>-lgort.

    IF <fl_mard>-labst IS INITIAL.

      wl_log-mensagem = TEXT-002.
      wl_log-tipo = 'E'.
      APPEND wl_log TO t_log.
      CLEAR: wl_log.
      v_nao_ok = 'X'.
      CONTINUE.

    ENDIF.

    READ TABLE tl_mchb TRANSPORTING NO FIELDS
                       WITH KEY matnr = <fl_mard>-matnr
                                werks = <fl_mard>-werks
                                lgort = <fl_mard>-lgort
                                BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT tl_mchb ASSIGNING FIELD-SYMBOL(<fl_mchb>) FROM sy-tabix.

        IF <fl_mchb>-matnr <> <fl_mard>-matnr OR
           <fl_mchb>-werks <> <fl_mard>-werks OR
           <fl_mchb>-lgort <> <fl_mard>-lgort.

          EXIT.

        ENDIF.

        CLEAR: wl_log-tipo, wl_log-mensagem.
        wl_log-charg = <fl_mchb>-charg.

        READ TABLE tl_iseg TRANSPORTING NO FIELDS
                           WITH KEY matnr = <fl_mchb>-matnr
                                    werks = <fl_mchb>-werks
                                    lgort = <fl_mchb>-lgort
                                    charg = <fl_mchb>-charg
                                    BINARY SEARCH.
        IF NOT sy-subrc IS INITIAL.

          IF NOT <fl_mchb>-clabs IS INITIAL.

            wl_log-clabs = <fl_mchb>-clabs.
            APPEND wl_log TO t_log.
            CLEAR: wl_log.
          ELSE.

            wl_log-tipo = 'W'.
            wl_log-mensagem = TEXT-003.
            APPEND wl_log TO t_log.
            CLEAR: wl_log.
          ENDIF.

        ELSE.

          wl_log-tipo = 'W'.
          wl_log-mensagem = TEXT-009.
          APPEND wl_log TO t_log.
          CLEAR: wl_log.
        ENDIF.


      ENDLOOP.

    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXECUTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_executar .

  DATA: wl_head             TYPE bapi_physinv_create_head,
        tl_itens            TYPE STANDARD TABLE OF bapi_physinv_create_items,
        wl_item             TYPE bapi_physinv_create_items,
        wl_header           TYPE bapi2017_gm_head_01,
*        TL_RETURN           TYPE STANDARD TABLE OF BAPIRET2,
*        TL_ITEM_GOODS       TYPE STANDARD TABLE OF BAPI2017_GM_ITEM_CREATE,
        wl_item_goods       TYPE bapi2017_gm_item_create,
        vl_materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year,
        vl_iblnr            TYPE iblnr,
        vl_msg              TYPE bapiret2-message,
        w_saldo             TYPE mchb-clabs.

  IF v_nao_ok IS INITIAL.


    REFRESH tl_return.

    CLEAR wl_header.
    wl_header-pstng_date = sy-datum.
    wl_header-doc_date = sy-datum.


    zc_selec_dados=>check_material(
      IMPORTING
        t_zppet013 = DATA(tl_zppet013)
        t_zppet014 = DATA(tl_zppet014)
    ).

    IF t_log_err IS NOT INITIAL.
      FREE: t_log.
      APPEND LINES OF t_log_err TO t_log.
      PERFORM zf_exibir_log.
      STOP.
    ELSE.
      APPEND LINES OF tl_zppet013 TO t_log.
      APPEND LINES OF tl_zppet014 TO t_log.
      SORT t_log BY matnr charg.
    ENDIF.

    IF t_log IS INITIAL.
      MESSAGE TEXT-004 TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    LOOP AT t_log ASSIGNING FIELD-SYMBOL(<fl_log>).
      IF <fl_log>-clabs IS INITIAL.
        <fl_log>-tipo = 'E'.
        <fl_log>-mensagem = TEXT-003.
      ENDIF.

      IF <fl_log>-tipo IS INITIAL.

        IF <fl_log>-charg IS NOT INITIAL.

          APPEND VALUE #( move_type   = 'ZI3'
                          plant       = <fl_log>-werks
                          material    = <fl_log>-matnr
                          entry_qnt   = <fl_log>-clabs
                          stge_loc    = <fl_log>-lgort
                          batch       = <fl_log>-charg
                          move_mat    = <fl_log>-matnr
                          move_plant  = <fl_log>-werks
                          move_stloc  = <fl_log>-lgort
                          move_batch  = 'INVENTARIO'
                             ) TO tl_item_goods.

          APPEND VALUE #( werks = <fl_log>-werks
                          matnr = <fl_log>-matnr
                          clabs = <fl_log>-clabs
                          lgort = <fl_log>-lgort
                          charg = <fl_log>-charg
                             ) TO tl_zppt0022.

*        ELSE.
*          CLEAR WL_ITEM_GOODS.
*          WL_ITEM_GOODS-MOVE_TYPE  = '311'.
*          WL_ITEM_GOODS-PLANT      = <FL_LOG>-WERKS.
*          WL_ITEM_GOODS-MATERIAL   = <FL_LOG>-MATNR.
*          WL_ITEM_GOODS-ENTRY_QNT  = <FL_LOG>-CLABS.
*          WL_ITEM_GOODS-STGE_LOC   = <FL_LOG>-LGORT.
*          APPEND WL_ITEM_GOODS TO TL_ITEM_GOODS.
        ENDIF.

      ENDIF.
    ENDLOOP.

    CHECK tl_item_goods IS NOT INITIAL.
    FREE: tl_return.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = wl_header
        goodsmvt_code    = '06'
      IMPORTING
        materialdocument = vl_materialdocument
        matdocumentyear  = vl_matdocumentyear
      TABLES
        goodsmvt_item    = tl_item_goods
        return           = tl_return.

    IF tl_return IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM tl_return COMPARING message.
    ENDIF.

    LOOP AT tl_return ASSIGNING FIELD-SYMBOL(<fl_return>).
      IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.
        vl_msg = <fl_return>-message.
        w_matnr = <fl_return>-message_v1.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF vl_msg IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MOVE-CORRESPONDING  tl_return TO t_msg.
      PERFORM verific_num_invetario.
      PERFORM zf_exibir_log.
    ENDIF.

    LOOP AT t_log ASSIGNING <fl_log>.

      IF <fl_log>-tipo IS INITIAL.

        IF NOT vl_msg IS INITIAL.
          <fl_log>-tipo = 'E'.
          <fl_log>-mensagem = vl_msg.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR:w_matnr.

    IF vl_msg IS NOT INITIAL.
      DELETE t_log WHERE tipo NE 'E'.
      EXIT.
    ELSE.
      CLEAR wl_head.
      wl_head-plant = p_werks.
      wl_head-stge_loc = p_lgort.
      wl_head-doc_date = sy-datum.
      wl_head-plan_date = sy-datum.
      wl_head-post_block = abap_true.

      LOOP AT t_log ASSIGNING <fl_log>.

        IF <fl_log>-tipo IS INITIAL.

          CLEAR wl_item.

* ---> S4 Migration - 06/07/2023 - FC
          "wl_item-material = <fl_log>-matnr.

          DATA(v_len) = strlen( <fl_log>-matnr ).

          IF v_len > 18.
            wl_item-material_long = <fl_log>-matnr.
          ELSE.
            wl_item-material = <fl_log>-matnr.
          ENDIF.
* <--- S4 Migration - 06/07/2023 - FC

          wl_item-batch = 'INVENTARIO'.
          APPEND wl_item TO tl_itens.
          CLEAR: wl_item.
        ENDIF.
      ENDLOOP.

      REFRESH tl_return.
      CLEAR vl_msg.

      DELETE ADJACENT DUPLICATES FROM tl_itens COMPARING material.

      CALL FUNCTION 'BAPI_MATPHYSINV_CREATE_MULT' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          head   = wl_head
        TABLES
          items  = tl_itens
          return = tl_return.

      LOOP AT tl_return ASSIGNING <fl_return>.

        IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

          vl_msg = <fl_return>-message.
          EXIT.

        ENDIF.
      ENDLOOP.

      IF vl_msg IS INITIAL.

        LOOP AT tl_return ASSIGNING <fl_return>.
          IF <fl_return>-type = 'S' AND <fl_return>-id = 'M7'
           AND <fl_return>-number = '710'.

            vl_iblnr = <fl_return>-message_v1.
            SHIFT vl_iblnr RIGHT DELETING TRAILING space.
            OVERLAY vl_iblnr WITH '0000000000'.
            EXIT.
          ENDIF.
        ENDLOOP.

        SELECT * INTO TABLE @DATA(tl_t0011)
        FROM zppt0011
        FOR ALL ENTRIES IN @t_log
        WHERE matnr = @t_log-matnr AND
              werks = @t_log-werks AND
              lgort = @t_log-lgort AND
              charg = @t_log-charg.


        SELECT * INTO TABLE @DATA(tl_t0016)
        FROM zppt0016
        FOR ALL ENTRIES IN @t_log
        WHERE matnr = @t_log-matnr AND
              werks = @t_log-werks AND
              lgort = @t_log-lgort AND
              charg = @t_log-charg.



        SORT tl_t0011 BY matnr werks lgort charg.
        LOOP AT t_log ASSIGNING <fl_log>.
          LOOP AT tl_t0011 ASSIGNING FIELD-SYMBOL(<fl_t0011>) WHERE matnr EQ <fl_log>-matnr
                                                               AND  werks EQ <fl_log>-werks
                                                               AND  lgort EQ <fl_log>-lgort
                                                               AND  charg EQ <fl_log>-charg.

            <fl_t0011>-iblnr = vl_iblnr.
            <fl_t0011>-gjahr = sy-datum(4).
            <fl_t0011>-mblnr_inv = vl_materialdocument.
            <fl_t0011>-mjahr_inv = vl_matdocumentyear.
            <fl_t0011>-mark = abap_true.
          ENDLOOP.

          LOOP AT tl_zppt0022 ASSIGNING FIELD-SYMBOL(<fl_t0022>) WHERE matnr EQ <fl_log>-matnr
                                                               AND  werks EQ <fl_log>-werks
                                                               AND  lgort EQ <fl_log>-lgort
                                                               AND  charg EQ <fl_log>-charg.

            <fl_t0022>-iblnr = vl_iblnr.
            <fl_t0022>-gjahr = sy-datum(4).
            <fl_t0022>-mblnr_inv = vl_materialdocument.
            <fl_t0022>-mjahr_inv = vl_matdocumentyear.
            <fl_t0022>-mark = abap_true.
          ENDLOOP.
        ENDLOOP.

        MODIFY zppt0011 FROM TABLE tl_t0011.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        MODIFY zppt0022 FROM TABLE tl_zppt0022.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      LOOP AT t_log ASSIGNING <fl_log>.

        IF <fl_log>-tipo IS INITIAL.

          IF NOT vl_msg IS INITIAL.

            <fl_log>-tipo = 'E'.
            <fl_log>-mensagem = vl_msg.

          ELSE.

            <fl_log>-tipo = 'S'.
            <fl_log>-iblnr   = vl_iblnr.
            <fl_log>-mensagem = TEXT-005.
*            CLEAR: VL_IBLNR.
          ENDIF.

        ENDIF.
      ENDLOOP.
      CLEAR: vl_iblnr.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_exibir_log .

  DATA: ol_alv       TYPE REF TO cl_salv_table,
        ol_salv_msg  TYPE REF TO cx_salv_msg,
        ol_functions TYPE REF TO cl_salv_functions_list,
        ol_layout    TYPE REF TO cl_salv_layout,
        ol_columns   TYPE REF TO cl_salv_columns_table,
*        OL_COLUMN         TYPE REF TO CL_SALV_COLUMN,
*        OL_SALV_NOT_FOUND TYPE REF TO CX_SALV_NOT_FOUND,
*        VL_LONGTEXT_L     TYPE SCRTEXT_L,
*        VL_LONGTEXT_S     TYPE SCRTEXT_S,
*        VL_LONGTEXT_M     TYPE SCRTEXT_M,
        vl_msg       TYPE string,
        wl_key       TYPE salv_s_layout_key.


  TRY.

      IF NOT rb_agru IS INITIAL.

*        IF W_RETURN NE 0.
*          MESSAGE TEXT-006 TYPE 'I'.
*          LEAVE LIST-PROCESSING.
*        ENDIF.

        IF t_msg IS NOT INITIAL.

          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = ol_alv
            CHANGING
              t_table      = t_msg.

        ELSE.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = ol_alv
            CHANGING
              t_table      = t_log.
        ENDIF.

      ELSE.
        IF t_desa_canc IS NOT INITIAL.
          APPEND LINES OF t_desa_canc TO t_desa.
        ENDIF.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = ol_alv
          CHANGING
            t_table      = t_desa.

      ENDIF.
    CATCH cx_salv_msg INTO ol_salv_msg.
      vl_msg = ol_salv_msg->get_longtext( ).
      MESSAGE vl_msg TYPE 'I'.
      LEAVE LIST-PROCESSING.

  ENDTRY.

  ol_functions = ol_alv->get_functions( ).
  ol_functions->set_all( cl_salv_functions_list=>true ).

  ol_layout = ol_alv->get_layout( ).
  wl_key-report = sy-repid.
  ol_layout->set_key( wl_key ).
  ol_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

  ol_columns = ol_alv->get_columns( ).
  ol_columns->set_optimize( cl_salv_columns_table=>true ).


  ol_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR_DESA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_selecionar_desa .

  DATA: lv_valido  TYPE flag,
        vl_msg     TYPE bapiret2-message,
        wl_headret TYPE  bapi2017_gm_head_ret,
        tl_return  TYPE STANDARD TABLE OF bapiret2,
        lw_desa    TYPE ty_desa.

  REFRESH t_desa.

  SELECT SINGLE iblnr, gjahr, vgart, invnu
         INTO @DATA(lw_ikpf)
         FROM ikpf
        WHERE iblnr = @p_iblnr AND
              gjahr = @p_gjahr.
  IF NOT sy-subrc IS INITIAL.

    MESSAGE TEXT-006 TYPE 'I'.
    LEAVE LIST-PROCESSING.

  ENDIF.

  SELECT iblnr, gjahr, zeili, matnr, werks, lgort, charg, sobkz, bstar, xdiff
         INTO TABLE @DATA(lt_iseg)
         FROM iseg
         WHERE iblnr = @p_iblnr AND
               gjahr = @p_gjahr.

  IF NOT lw_ikpf-invnu IS INITIAL.

    SELECT iblnr, gjahr, zeili, matnr, werks, lgort, charg, sobkz, bstar, xdiff
           APPENDING TABLE @lt_iseg
           FROM iseg
           WHERE iblnr = @lw_ikpf-invnu AND
                 gjahr = @p_gjahr.

  ENDIF.

  lv_valido = 'X'.
  LOOP AT lt_iseg ASSIGNING FIELD-SYMBOL(<lf_iseg>).

    CLEAR lw_desa.

    lw_desa-gjahr = <lf_iseg>-gjahr.
    lw_desa-zeili = <lf_iseg>-zeili.
    lw_desa-matnr = <lf_iseg>-matnr.
    lw_desa-werks = <lf_iseg>-werks.
    lw_desa-lgort = <lf_iseg>-lgort.
    lw_desa-charg = <lf_iseg>-charg.
    lw_desa-sobkz = <lf_iseg>-sobkz.
    lw_desa-invnu = lw_ikpf-invnu.

    IF <lf_iseg>-iblnr <> lw_ikpf-invnu.

      lw_desa-iblnr = <lf_iseg>-iblnr.
      IF <lf_iseg>-xdiff IS INITIAL.

        lw_desa-tipo = 'E'.
        lw_desa-mensagem = TEXT-007.
        APPEND lw_desa TO t_desa.
        CLEAR lv_valido.

      ELSE.

        APPEND lw_desa TO t_desa.

      ENDIF.

    ELSE.

      lw_desa-iblnr = lw_ikpf-iblnr.
      APPEND lw_desa TO t_desa.

    ENDIF.

  ENDLOOP.

  IF NOT lv_valido IS INITIAL.

    SELECT * INTO TABLE @DATA(tl_t0011)
           FROM zppt0011
           WHERE iblnr = @p_iblnr AND
                 gjahr = @p_gjahr.

    IF NOT lw_ikpf-invnu IS INITIAL.

      SELECT * APPENDING TABLE @tl_t0011
             FROM zppt0011
             WHERE iblnr = @lw_ikpf-invnu AND
                   gjahr = @p_gjahr.

    ENDIF.

    READ TABLE tl_t0011 ASSIGNING FIELD-SYMBOL(<fl_t0011>) INDEX 1.
    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument = <fl_t0011>-mblnr_inv
          matdocumentyear  = <fl_t0011>-mjahr_inv
        IMPORTING
          goodsmvt_headret = wl_headret
        TABLES
          return           = tl_return[].


      LOOP AT tl_return ASSIGNING FIELD-SYMBOL(<fl_return>).

        IF <fl_return>-type = 'E' OR <fl_return>-type = 'A'.

          vl_msg = <fl_return>-message.
          EXIT.

        ENDIF.

      ENDLOOP.

      IF vl_msg IS INITIAL.

        LOOP AT tl_t0011 ASSIGNING <fl_t0011>.

          CLEAR: <fl_t0011>-mblnr_inv, <fl_t0011>-mjahr_inv, <fl_t0011>-iblnr, <fl_t0011>-gjahr.

        ENDLOOP.

        MODIFY zppt0011 FROM TABLE tl_t0011.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.


      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


      ENDIF.

    ENDIF.


    LOOP AT t_desa ASSIGNING FIELD-SYMBOL(<fl_desa>).

      IF <fl_desa>-tipo IS INITIAL.

        IF NOT vl_msg IS INITIAL.

          <fl_desa>-tipo = 'E'.
          <fl_desa>-mensagem = vl_msg.

        ELSE.

          <fl_desa>-tipo = 'S'.
          <fl_desa>-mensagem = TEXT-008.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VERIFIC_NUM_INVETARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verific_num_invetario .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TIT0200'.

  REFRESH it_fcat.
  it_fcat = zc_selec_dados=>get_fieldcatalog( ).
*  DATA(_LAYOUT)       = VALUE LVC_S_LAYO( STYLEFNAME = 'STYLE' ).
  DATA(_botao_alv)    = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
  DATA: grid_event    TYPE REF TO lcl_event_handler.


******  Adicionando linhas.

  DATA cont TYPE int4 VALUE 0.
*
*  PERFORM ALV_PREENCHE_CAT USING:
**  02 'GJAHR   ' '' '10' ''  ''  ' ' 'Exercício          ',
*  03 'MATNR   ' '' '14' ''  ''  ' ' 'Nº do material     ',
*  04 'WERKS   ' '' '13' ''  ''  ' ' 'Centro             ',
*  05 'LGORT   ' '' '13' ''  ''  ' ' 'Depósito           ',
*  06 'CHARG   ' '' '11' ''  ''  ' ' 'Lote origem        ',
*  07 'W_CHARG ' '' '11' 'X' ''  ' ' 'Lote destino       ',
*  08 'CLABS   ' '' '11' 'X' ''  'X ' 'Volume             ',
*  09 'STATUS  ' '' '11' 'X' ''  'X ' 'Status             '.
**  08 'CLABS   ' '' '11' ''  ''  'X' 'utilização livre   '. // APPEND WA_TELA TO T_TRANSF.


  PERFORM fill_it_sort.

  wa_layout-excp_conds     = 'X'.
  wa_layout-zebra          = 'X'.
  wa_layout-sel_mode       = 'A'.
  wa_layout-cwidth_opt     = 'X'.     "  Otimizar colunas na tela
  wa_layout-totals_bef     = ' '.

  IF t_transf IS NOT INITIAL.

    IF cont NE wa_tela-qtd.
      FREE: tg_itens.
      CLEAR: cont.
      DATA(linha) = lines( tg_itens ).
      ADD linha TO cont.
      WHILE cont < wa_tela-qtd.
        APPEND VALUE #(
                        matnr = wa_tela-matnr
                        charg = wa_tela-charg
                        werks = wa_tela-werks
                         lgort  = wa_tela-lgort
                         gjahr = wa_tela-gjahr
                      ) TO tg_itens.
        ADD 1 TO cont.
      ENDWHILE.
    ENDIF.
  ELSE.
    FREE: tg_itens.
    APPEND LINES OF t_transf TO tg_itens.
  ENDIF.

  IF custom_grid IS NOT INITIAL.
    CALL METHOD grid->free.
    CALL METHOD custom_grid->free.
  ENDIF.

  CREATE OBJECT custom_grid
    EXPORTING
      container_name = 'CONTAINER'.

  CREATE OBJECT grid
    EXPORTING
      i_parent = custom_grid.

  CALL METHOD grid->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = _botao_alv
      is_layout            = wa_layout
      i_save               = abap_true
    CHANGING
      it_outtab            = tg_itens
      it_fieldcatalog      = it_fcat.

  CALL METHOD grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER: grid_event->on_data_changed FOR grid.

  CALL METHOD grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      FREE: t_erro.
      zc_selec_dados=>check_cadastro_material( ).

      IF t_erro IS NOT INITIAL.
        FREE: tg_itens.
        APPEND LINES OF t_erro TO tg_itens.
      ELSE.
        zc_selec_dados=>set_material( ).

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_sort .

  DATA: wa_sort TYPE lvc_s_sort.

  wa_sort-spos = '1'.
  wa_sort-fieldname = 'WERKS'.
  "WA_SORT-DOWN = 'X'.
  wa_sort-group = '*'.
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_01     text
*      -->P_3669   text
*      -->P_3670   text
*      -->P_3671   text
*      -->P_3672   text
*      -->P_3673   text
*      -->P_3674   text
*      -->P_3675   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING: VALUE(p_colnum)
                                VALUE(p_fieldname)
                                VALUE(p_tabname)
                                VALUE(p_len)
                                VALUE(p_edit)
                                VALUE(p_icon)
                                VALUE(p_do_sum)
                                VALUE(p_header).

  DATA: wl_fcat TYPE lvc_s_fcat.
  CLEAR: wa_layout, wl_fcat.

  wl_fcat-col_pos     = p_colnum.
  wl_fcat-fieldname   = p_fieldname.
  wl_fcat-tabname     = p_tabname.
  wl_fcat-outputlen   = p_len.
  wl_fcat-coltext     = p_header.
  wl_fcat-edit        = p_edit.
  wl_fcat-icon        = p_icon.
  wl_fcat-ref_table   = p_tabname.
  wl_fcat-checktable  = p_tabname.
  wl_fcat-do_sum      = p_do_sum.
  APPEND wl_fcat TO it_fcat.

ENDFORM.
