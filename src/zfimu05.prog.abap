*&--------------------------------------------------------------------&*
*&                        FI                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento / Antonio Rodrigues                    &*
*& Data.....: 10/05/2016                                              &*
*& Descrição: Preenchimento Tabela contrato mutuo                     &*
*& Transação: ZFIMU13                                                 &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Request DEVK956995     Data 10/05/2016  IR IR98191                 &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zfimu05.
*=============================================================================*
*TABELAS
*=============================================================================*
TABLES: bsik.

DATA: BEGIN OF holiday OCCURS 0.
        INCLUDE STRUCTURE iscal_day.
      DATA END OF holiday.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
      DATA: END OF it_msg.

TYPES: BEGIN OF ty_saida,
         zid_empresa   TYPE zfictrmu-zid_empresa,
         des_empresa   TYPE t001-butxt,
         zid_contr     TYPE zfictrmu-zid_contr,
         nom_contr     TYPE zfitaxctr-nom_contr,
         nro_sol       TYPE zfisolctr-nro_sol,
         taxa          TYPE zfitaxctr-taxa,
         moda(1),
         zid_razao     TYPE zfitaxctr-hkont_f,
         zid_forcli    TYPE zfitaxctr-lifnr,
         zvinc_cia     TYPE zfictrmu-zvinc_cia,
         buzei         TYPE zfictrmu-buzei,
         zvlor_princ   TYPE bseg-wrbtr, "ZFICTRMU-ZVLOR_PRINC,
         zmoeda        TYPE zfictrmu-zmoeda,
         zdata_mutuo   TYPE zfictrmu-zdata_mutuo,
         budat         TYPE zfictrmu-zdata_mutuo,
         gjahr         TYPE bseg-gjahr,
         line_color(4) TYPE c, "Used to store row color attributes
       END OF ty_saida.


TYPES: BEGIN OF ty_saida_calculo,
         zid_empresa    TYPE zfictrmu-zid_empresa,
         des_empresa    TYPE t001-butxt,
         zid_contr      TYPE zfirescmu-zid_contr,
         zres_calc_inic TYPE zfirescmu-zresdt_calc,
         zresdt_calc    TYPE zfirescmu-zresdt_calc,
         zresvl_princ   TYPE bseg-wrbtr, "ZFIRESCMU-ZRESVL_PRINC,
         zresid_juros   TYPE bseg-wrbtr, "ZFIRESCMU-ZRESID_JUROS,
         zresid_iof     TYPE bseg-wrbtr, "ZFIRESCMU-ZRESID_IOF,
         zresid_ir      TYPE bseg-wrbtr, "ZFIRESCMU-ZRESID_IR,
         zresid_result  TYPE bseg-wrbtr, "ZFIRESCMU-ZRESID_RESULT,
         zresid_resultj TYPE bseg-wrbtr, "ZFIRESCMU-ZRESID_RESULT,
         doc_lcto       TYPE zfirescmu-doc_lcto,
         belnr_res      TYPE zfirescmu-belnr_res,
         belnr_resj     TYPE zfirescmu-belnr_resj,
         belnr          TYPE zib_contabil_chv-belnr,
         gjahr          TYPE bseg-gjahr,
       END OF ty_saida_calculo.

TYPES:BEGIN OF ty_zfisolctr.
        INCLUDE STRUCTURE zfisolctr.
        TYPES:  obj_keyf TYPE awkey,
        obj_keyc TYPE awkey.
TYPES: END OF ty_zfisolctr.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA: it_saida                TYPE TABLE OF ty_saida,
      it_saida_calculo        TYPE TABLE OF ty_saida_calculo,
      it_saida_memoria        TYPE TABLE OF zficalcmu,

      it_bsik_aux             TYPE TABLE OF bsik,
      it_zfitaxctr            TYPE TABLE OF zfitaxctr,
      it_zfisolctr            TYPE TABLE OF ty_zfisolctr,
      it_bsid_aux             TYPE TABLE OF bsid,

      it_t001                 TYPE TABLE OF t001,
      it_lfa1                 TYPE TABLE OF lfa1,
      it_kna1                 TYPE TABLE OF kna1,
      it_zfit0071             TYPE TABLE OF zfit0071,
      it_bseg                 TYPE TABLE OF bseg,
      wa_bseg                 TYPE bseg,

      it_zficalcmu            TYPE TABLE OF zficalcmu,

      it_zfictrmu             TYPE TABLE OF zfictrmu,
      it_zfictrmu_atual       TYPE TABLE OF zfictrmu,
      it_zfirescmu            TYPE TABLE OF zfirescmu,
      it_zficalenmu           TYPE TABLE OF zficalenmu,
      it_zib_contabil_chv_aux TYPE TABLE OF zib_contabil_chv.


DATA: it_zib_contabil_chv TYPE TABLE OF zib_contabil_chv,
      wa_zib_contabil_chv TYPE zib_contabil_chv,

      ti_bdcdata          TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata          LIKE LINE OF ti_bdcdata,
      t_messtab           TYPE TABLE OF bdcmsgcoll.

*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: wa_saida                TYPE ty_saida,
      wa_saida_calculo        TYPE ty_saida_calculo,

      wa_bsik                 TYPE bsik,
      wa_bsik_aux             TYPE bsik,
      wa_bsid                 TYPE bsid,
      wa_bsid_aux             TYPE bsid,
      wa_zfitaxctr            TYPE zfitaxctr,
      wa_zfisolctr            TYPE ty_zfisolctr,

      wa_t001                 TYPE t001,
      wa_lfa1                 TYPE lfa1,
      wa_kna1                 TYPE kna1,
      wa_zfit0071             TYPE zfit0071,

      wa_zficalcmu            TYPE zficalcmu,
      wa_zfirescmu            TYPE zfirescmu,
      wa_zfitaxcdimu          TYPE zfitaxcdimu,
      wa_zfialirmu            TYPE zfialirmu,

      wa_zfictrmu             TYPE zfictrmu,
      wa_zfictrmu_atual       TYPE zfictrmu,
      wa_zficalenmu           TYPE zficalenmu,
      wa_zfialiofmu           TYPE zfialiofmu,

      wa_zib_contabil_chv_aux TYPE zib_contabil_chv.


DATA: wa_cont     TYPE REF TO cl_gui_custom_container,
      wa_alv      TYPE REF TO cl_gui_alv_grid,
      wa_cont_02  TYPE REF TO cl_gui_custom_container,
      wa_cont_03  TYPE REF TO cl_gui_custom_container,
      wa_cont_04  TYPE REF TO cl_gui_custom_container,
      wa_cont_05  TYPE REF TO cl_gui_custom_container,
      wa_alv_02   TYPE REF TO cl_gui_alv_grid,
      wa_alv_03   TYPE REF TO cl_gui_alv_grid,
      wa_alv_04   TYPE REF TO cl_gui_alv_grid,
      wa_alv_05   TYPE REF TO cl_gui_alv_grid,
      it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row,
      ty_toolbar  TYPE stb_button,
      it_fcat     TYPE lvc_t_fcat,
      wa_fcat     TYPE lvc_s_fcat,
      it_fcat_02  TYPE lvc_t_fcat,
      it_fcat_03  TYPE lvc_t_fcat,
      it_fcat_04  TYPE lvc_t_fcat,
      it_fcat_05  TYPE lvc_t_fcat,
      wa_fcat_02  TYPE lvc_s_fcat,
      wa_fcat_03  TYPE lvc_s_fcat,
      wa_fcat_04  TYPE lvc_s_fcat,
      wa_fcat_05  TYPE lvc_s_fcat,
      it_sort     TYPE lvc_t_sort,
      wa_sort     TYPE lvc_s_sort,
      wa_variante TYPE disvariant,
      wa_stable   TYPE lvc_s_stbl,
      wa_layout   TYPE lvc_s_layo,
      c_alv_tm    TYPE REF TO cl_alv_grid_toolbar_manager,
      gt_estilo   TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo   TYPE lvc_s_styl,
      it_dta      TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      wa_dta      TYPE bdcdata,
      opt         TYPE ctu_params.

FIELD-SYMBOLS: <saida> TYPE ty_saida,
               <fcat>  TYPE lvc_s_fcat.

DATA: x_screen TYPE sy-dynnr VALUE '0300'.
DATA  empresa TYPE char140.
DATA  tp_lanc.
DATA  tela_lanc.
DATA  entrada_lanc(30).
DATA  lanc.
DATA  bt_01(5).
DATA  bt_02(5).
DATA  var_doc_res TYPE zglt035-doc_lcto.
DATA  vg_i       TYPE i.
DATA  vg_last_day     TYPE sy-datum.
DATA  wg_documento(10).

SELECTION-SCREEN: BEGIN OF SCREEN 0102 AS SUBSCREEN .

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (33) text-su1 FOR FIELD p_ctr.
PARAMETER p_ctr  TYPE zfisolctr-zid_contr .
SELECTION-SCREEN COMMENT (55)  vtxt.
SELECTION-SCREEN END OF LINE.

PARAMETER p_data_i TYPE zfisolctr-dt_lcto .
PARAMETER p_data   TYPE zfisolctr-dt_lcto .
*PARAMETER P_TAX    TYPE ZFIT0060-TX_PAR_DOLAR .
SELECTION-SCREEN: END OF SCREEN 0102.

AT SELECTION-SCREEN OUTPUT.

  SELECT SINGLE *
  FROM zfitaxctr
  INTO wa_zfitaxctr
  WHERE zid_contr EQ p_ctr.
  vtxt = wa_zfitaxctr-nom_contr.


INITIALIZATION.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ctr.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_ctr OCCURS 0,
          zid_contr TYPE  zfitaxctr-zid_contr,
          nom_contr TYPE  zfitaxctr-nom_contr,
          bukrs_f   TYPE  zfitaxctr-bukrs_f,
          lifnr     TYPE  zfitaxctr-lifnr,
          name1_f   TYPE  lfa1-name1,
          bukrs_c   TYPE  zfitaxctr-bukrs_c,
          kunnr     TYPE  zfitaxctr-kunnr,
          name1_c   TYPE  kna1-name1,
          dt_ini    TYPE  zfitaxctr-dt_ini,
          dt_fim    TYPE  zfitaxctr-dt_fim,
          taxa      TYPE  zfitaxctr-taxa,
          waers     TYPE  zfitaxctr-waers,
        END OF tl_ctr.

  SELECT zfitaxctr~zid_contr
         zfitaxctr~nom_contr
         zfitaxctr~bukrs_f
         zfitaxctr~lifnr
         lfa1~name1
         zfitaxctr~bukrs_c
         zfitaxctr~kunnr
         kna1~name1
         zfitaxctr~dt_ini
         zfitaxctr~dt_fim
         zfitaxctr~taxa
         zfitaxctr~waers
    FROM zfitaxctr
    LEFT OUTER JOIN lfa1
    ON lfa1~lifnr EQ zfitaxctr~lifnr
    LEFT OUTER JOIN kna1
    ON kna1~kunnr EQ zfitaxctr~kunnr
    INTO TABLE tl_ctr
    WHERE dt_ini LE sy-datum
    AND   dt_fim GE sy-datum.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZID_CONTR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFITAXCTR-ZID_CONTR'
      value_org       = 'S'
    TABLES
      value_tab       = tl_ctr
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

*       CLASS ZCL_EVENTS DEFINITION
CLASS zcl_events DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid  TYPE REF TO cl_gui_alv_grid,
      on_click    FOR EVENT hotspot_click         OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "ZCL_EVENTS DEFINITION
*       CLASS ZCL_CONTROL_NDF DEFINITION
CLASS zcl_control_mutuos DEFINITION.
  PUBLIC SECTION.
    METHODS:
      seleciona_dados              ,
      inserir_novo_mutuo           ,
      montar_alv                   ,
      montar_lay                   ,
      cria_alv                     ,
      mutuos_gerado                ,
      gera_lote                    ,
      refresh_dados.

    DATA: go_utils     TYPE REF TO zcl_control_mutuos,
          r_gerar_lote TYPE REF TO zcl_gerar_lote.

ENDCLASS.                    "ZCL_CONTROL_NDF DEFINITION
*=============================================================================*
*TABELAS
*=============================================================================*
DATA: obg_events  TYPE REF TO zcl_events,
      obg_control TYPE REF TO zcl_control_mutuos.

*       CLASS ZCL_EVENTS IMPLEMENTATION
CLASS zcl_events IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_tm
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONTRUCTOR

  METHOD on_click.
    DATA: vg_lote             TYPE zglt034-lote.

    CHECK e_row_id-rowtype IS INITIAL.

    CHECK e_row_id-index IS NOT INITIAL.

    CASE e_column_id.
      WHEN 'ZVINC_CIA'.
        CHECK it_saida[] IS NOT INITIAL.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        CHECK ( wa_saida-zvinc_cia IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD wa_saida-zvinc_cia.
        SET PARAMETER ID 'BUK' FIELD wa_saida-zid_empresa.
        SET PARAMETER ID 'GJR' FIELD wa_saida-zdata_mutuo(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'BELNR'.
        CHECK it_saida_calculo[] IS NOT INITIAL.
        READ TABLE it_saida_calculo INTO wa_saida_calculo INDEX e_row_id-index.
        CHECK ( wa_saida_calculo-belnr IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD wa_saida_calculo-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida_calculo-zid_empresa.
        SET PARAMETER ID 'GJR' FIELD p_data(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'BELNR_RES'.
        CHECK it_saida_calculo[] IS NOT INITIAL.
        READ TABLE it_saida_calculo INTO wa_saida_calculo INDEX e_row_id-index.
        CHECK ( wa_saida_calculo-belnr_res IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD wa_saida_calculo-belnr_res.
        SET PARAMETER ID 'BUK' FIELD wa_saida_calculo-zid_empresa.
        SET PARAMETER ID 'GJR' FIELD p_data(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'BELNR_RESJ'.
        CHECK it_saida_calculo[] IS NOT INITIAL.
        READ TABLE it_saida_calculo INTO wa_saida_calculo INDEX e_row_id-index.
        CHECK ( wa_saida_calculo-belnr_resj IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD wa_saida_calculo-belnr_resj.
        SET PARAMETER ID 'BUK' FIELD wa_saida_calculo-zid_empresa.
        SET PARAMETER ID 'GJR' FIELD p_data(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      WHEN 'DOC_LCTO'.
        CHECK it_saida_calculo[] IS NOT INITIAL.
        READ TABLE it_saida_calculo INTO wa_saida_calculo INDEX e_row_id-index.
        CLEAR vg_lote.
        CHECK ( wa_saida_calculo-doc_lcto IS NOT INITIAL ).
        SET PARAMETER ID 'BLN' FIELD wa_saida_calculo-doc_lcto.
        SET PARAMETER ID 'LOT' FIELD  vg_lote.
        CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD.                    "ON_CLICK

ENDCLASS.                    "ZCL_EVENTS IMPLEMENTATION
*=============================================================================*
*CLASS ZCL_CONTROL_MUTUOS IMPLEMENTATION.
*=============================================================================*
CLASS zcl_control_mutuos IMPLEMENTATION.

*=============================================================================*
*TABELAS
*=============================================================================*
  METHOD seleciona_dados.
    DATA: tabix               TYPE sy-tabix,
          vg_first_day_aux(8),
          vg_first_day        TYPE sy-datum.

    CHECK p_ctr IS NOT INITIAL.

    FREE: it_bsik_aux, it_bsid_aux.

    REFRESH: it_zfictrmu.

    SELECT SINGLE *
      FROM zfitaxctr
      INTO wa_zfitaxctr
      WHERE zid_contr EQ p_ctr.

    IF sy-subrc NE 0.
      MESSAGE 'Favor verificar o campo Contrato' TYPE 'I'.
      EXIT.
    ENDIF.

    IF wa_zfitaxctr-dt_fim LT p_data.
      MESSAGE 'Contrato encerrado' TYPE 'I'.
      EXIT.
    ENDIF.

    IF  p_data_i IS INITIAL.
      CONCATENATE p_data+0(6) '01' INTO vg_first_day_aux.
      vg_first_day = vg_first_day_aux.
    ELSE.
      vg_first_day = p_data_i.
    ENDIF.


    SELECT *
     FROM  zfisolctr
     INTO TABLE it_zfisolctr
    WHERE zid_contr EQ p_ctr
    AND   dt_lcto   BETWEEN vg_first_day AND p_data.

    LOOP AT it_zfisolctr INTO wa_zfisolctr.
      IF wa_zfisolctr-bukrs_f = '9999'.
        CLEAR wa_zfisolctr-obj_keyf.
      ELSE.
        CONCATENATE 'ZGL17' wa_zfisolctr-doc_lctof wa_zfisolctr-dt_lcto+0(4) INTO wa_zfisolctr-obj_keyf.
      ENDIF.
      IF wa_zfisolctr-bukrs_c = '9999'.
        CLEAR wa_zfisolctr-obj_keyc.
      ELSE.
        CONCATENATE 'ZGL17' wa_zfisolctr-doc_lctoc wa_zfisolctr-dt_lcto+0(4) INTO wa_zfisolctr-obj_keyc.
      ENDIF.
      MODIFY it_zfisolctr FROM wa_zfisolctr INDEX sy-tabix TRANSPORTING obj_keyf obj_keyc.
    ENDLOOP.

    IF it_zfisolctr[] IS NOT INITIAL.
      SELECT  *
         FROM zib_contabil_chv
         INTO TABLE it_zib_contabil_chv
         FOR ALL ENTRIES IN it_zfisolctr
         WHERE obj_key EQ it_zfisolctr-obj_keyf.

      SELECT  *
       FROM zib_contabil_chv
       APPENDING TABLE it_zib_contabil_chv
       FOR ALL ENTRIES IN it_zfisolctr
       WHERE obj_key EQ it_zfisolctr-obj_keyc.

      SORT it_zib_contabil_chv BY obj_key.

      LOOP AT it_zfisolctr INTO wa_zfisolctr.
        tabix = sy-tabix.
        "docs fornecedor
        READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zfisolctr-obj_keyf BINARY SEARCH.
        IF sy-subrc = 0.
          wa_zfisolctr-belnrf = wa_zib_contabil_chv-belnr.
          wa_zfisolctr-gjahrf = wa_zib_contabil_chv-gjahr.
        ENDIF.
        "docs cliente
        READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_zfisolctr-obj_keyc BINARY SEARCH.
        IF sy-subrc = 0.
          wa_zfisolctr-belnrc = wa_zib_contabil_chv-belnr.
          wa_zfisolctr-gjahrc = wa_zib_contabil_chv-gjahr.
        ENDIF.
        MODIFY it_zfisolctr FROM wa_zfisolctr INDEX tabix TRANSPORTING belnrf gjahrf belnrc gjahrc.
      ENDLOOP.
    ENDIF.

    "Eliminar se não houver lancamento contabil nas duas empresas
    DELETE it_zfisolctr  WHERE  belnrf IS INITIAL AND  bukrs_f NE '9999'.
    DELETE it_zfisolctr  WHERE  belnrc IS INITIAL AND  bukrs_c NE '9999'.

    IF it_zfisolctr[] IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_bsid_aux
        FROM bsid
        INNER JOIN bkpf
        ON  bkpf~bukrs EQ bsid~bukrs
        AND bkpf~belnr EQ bsid~belnr
        AND bkpf~gjahr EQ bsid~gjahr
        AND bkpf~stblg EQ ''
        FOR ALL ENTRIES IN it_zfisolctr
        WHERE bsid~bukrs = it_zfisolctr-bukrs_c
        AND   bsid~belnr = it_zfisolctr-belnrc
        AND   bsid~gjahr = it_zfisolctr-gjahrc
        AND   bsid~hkont = it_zfisolctr-hkont_c
        AND   bsid~belnr NE bsid~augbl.

      IF it_bsid_aux[] IS NOT INITIAL.
        SELECT *
          FROM zfictrmu
          INTO TABLE it_zfictrmu
          FOR ALL ENTRIES IN it_bsid_aux
          WHERE zid_contr EQ p_ctr
          AND   zvinc_cia EQ it_bsid_aux-belnr
          AND   doc_lcto  EQ ''.  "não gerou docto, pode pegar valor da tabela standard

        SELECT *
          FROM kna1
          INTO TABLE it_kna1
          FOR ALL ENTRIES IN it_bsid_aux
          WHERE lifnr EQ it_bsid_aux-kunnr.

        SELECT *
         FROM t001
         INTO TABLE it_t001
         FOR ALL ENTRIES IN it_bsid_aux
         WHERE bukrs EQ it_bsid_aux-bukrs.

        CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSID_AUX
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND UMSKZ EQ '' AND AUGBL NE ''|
    IMPORTING ET_BSEG = IT_BSEG
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC <> 0 OR LINES( IT_BSEG ) = 0.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ELSE.
  SY-DBCNT = LINES( IT_BSEG ).
ENDIF.


      ENDIF.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_bsik_aux
        FROM bsik
        INNER JOIN bkpf
        ON  bkpf~bukrs EQ bsik~bukrs
        AND bkpf~belnr EQ bsik~belnr
        AND bkpf~gjahr EQ bsik~gjahr
        AND bkpf~stblg EQ ''
        FOR ALL ENTRIES IN it_zfisolctr
        WHERE bsik~bukrs = it_zfisolctr-bukrs_f
        AND   bsik~belnr = it_zfisolctr-belnrf
        AND   bsik~gjahr = it_zfisolctr-gjahrf
        AND   bsik~hkont = it_zfisolctr-hkont_f
        AND   bsik~belnr NE bsik~augbl.

      IF it_bsik_aux[] IS NOT INITIAL.
        SELECT *
          FROM zfictrmu
          APPENDING TABLE it_zfictrmu
          FOR ALL ENTRIES IN it_bsik_aux
          WHERE zid_contr EQ p_ctr
          AND   zvinc_cia EQ it_bsik_aux-belnr
          AND   doc_lcto  EQ ''. "não gerou docto, pode pegar valor da tabela standard

        SELECT *
            FROM lfa1
            INTO TABLE it_lfa1
            FOR ALL ENTRIES IN it_bsik_aux
            WHERE lifnr EQ it_bsik_aux-lifnr.

        SELECT *
           FROM t001
           APPENDING TABLE it_t001
           FOR ALL ENTRIES IN it_bsik_aux
           WHERE bukrs EQ it_bsik_aux-bukrs.

        DATA ETL546C8R361 TYPE TABLE OF BSEG.
CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSIK_AUX
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND UMSKZ EQ '' AND AUGBL NE ''|
    IMPORTING ET_BSEG = ETL546C8R361
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL546C8R361 ) > 0.
  APPEND LINES OF ETL546C8R361 TO IT_BSEG.
  SY-DBCNT = LINES( ETL546C8R361 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.

      ENDIF.
    ELSE.
      MESSAGE 'Solicitações não encontrados, verificar o campo Contrato/Data' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT *
      FROM zfictrmu
      INTO TABLE it_zfictrmu_atual
      WHERE zid_contr     = p_ctr
      AND   zresdt_calc   = p_data
      AND   doc_lcto  NE ''.  "já gerou docto

    IF it_zfictrmu_atual[] IS NOT INITIAL.
      SELECT *
        FROM t001
        APPENDING TABLE it_t001
        FOR ALL ENTRIES IN it_zfictrmu_atual
        WHERE bukrs EQ it_zfictrmu_atual-zid_empresa.

      SELECT *
         FROM lfa1
         APPENDING TABLE it_lfa1
         FOR ALL ENTRIES IN it_zfictrmu_atual
         WHERE lifnr EQ it_zfictrmu_atual-lifnr.

      SELECT *
         FROM kna1
         APPENDING TABLE it_kna1
         FOR ALL ENTRIES IN it_zfictrmu_atual
         WHERE lifnr EQ it_zfictrmu_atual-kunnr.
    ENDIF.

  ENDMETHOD.                    "SELECIONA_DADOS
*=============================================================================*
* METHOD INSERIR_NOVO_MUTUO.
*=============================================================================*
  METHOD inserir_novo_mutuo.

    DATA: wa_zfictrmu_aux             TYPE zfictrmu.
    REFRESH: it_saida.
    SORT: it_zfictrmu BY zvinc_cia,
          it_zfictrmu_atual BY zvinc_cia,
          it_bseg BY bukrs belnr gjahr.

    SORT it_zfisolctr BY belnrc.

    "Novos documentos Cliente
    LOOP AT it_bsid_aux INTO wa_bsid_aux.

      READ TABLE it_zfictrmu_atual INTO wa_zfictrmu_atual WITH KEY zvinc_cia = wa_bsid_aux-belnr BINARY SEARCH. "gerou docto ZGL, não pega novamente
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR wa_zfictrmu.

      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_bsid_aux-bukrs.
      READ TABLE it_zfisolctr INTO wa_zfisolctr WITH KEY belnrc = wa_bsid_aux-belnr BINARY SEARCH.
      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_bsid_aux-kunnr.

      wa_zfictrmu-zid_contr       = p_ctr.
      wa_zfictrmu-zid_empresa     = wa_bsid_aux-bukrs.
      wa_zfictrmu-zresdt_calc     = p_data.
      wa_zfictrmu-zvinc_cia       = wa_bsid_aux-belnr.
      wa_zfictrmu-buzei           = wa_bsid_aux-buzei.
      wa_zfictrmu-gjahr           = wa_bsid_aux-gjahr.
      wa_zfictrmu-nro_sol         = wa_zfisolctr-nro_sol.
      IF wa_bsid_aux-shkzg = 'H'.
        wa_zfictrmu-zvlor_princ   = wa_bsid_aux-wrbtr * -1.
      ELSE.
        wa_zfictrmu-zvlor_princ   = wa_bsid_aux-wrbtr.
      ENDIF.
      wa_zfictrmu-zmoeda          = wa_bsid_aux-pswsl.
      wa_zfictrmu-kunnr           = wa_bsid_aux-kunnr.
      wa_zfictrmu-usnam        = sy-uname.
      wa_zfictrmu-dt_entrada   = sy-datum.
      wa_zfictrmu-hr_entrada   = sy-uzeit.

      MOVE-CORRESPONDING wa_zfictrmu TO wa_saida.
      wa_saida-budat           = wa_bsid_aux-budat.
      wa_saida-gjahr           = wa_bsid_aux-gjahr.
      wa_saida-taxa            = wa_zfitaxctr-taxa.
      wa_saida-des_empresa     = wa_t001-butxt.
      wa_saida-zid_razao       = wa_zfitaxctr-hkont_c.
      wa_saida-zid_forcli      = wa_zfitaxctr-kunnr.

      CLEAR: wa_zfictrmu-zdata_mutuo, wa_saida-line_color.
      IF wa_zfisolctr-cd_mod NE 'R'.
        CLEAR wa_bseg.
        READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsid_aux-bukrs
                                                 belnr = wa_bsid_aux-belnr
                                                 gjahr = wa_bsid_aux-gjahr BINARY SEARCH.
        IF wa_bseg-augbl IS INITIAL.
          wa_saida-line_color      = 'C611'.
          wa_zfictrmu-zdata_mutuo = wa_bsid_aux-budat.
        ELSE.
          wa_zfictrmu-zdata_mutuo     = wa_bseg-augdt.
        ENDIF.
      ELSE.
        wa_zfictrmu-zdata_mutuo     = wa_bsid_aux-budat.
      ENDIF.
      wa_saida-zdata_mutuo = wa_zfictrmu-zdata_mutuo.

      IF wa_zfitaxctr-waers NE 'BRL'. "Juros simples
        IF wa_zfisolctr-cd_mod = 'J'.
          wa_saida-moda = 'J'.
        ELSE.
          wa_saida-moda = 'P'.
        ENDIF.
      ENDIF.
      "
      APPEND wa_saida    TO it_saida.

      READ TABLE it_zfictrmu INTO wa_zfictrmu_aux WITH KEY zvinc_cia = wa_bsid_aux-belnr BINARY SEARCH.
      IF sy-subrc NE 0.
        IF  wa_zfictrmu-doc_lcto IS INITIAL."Se não gerou lançamento contabil pode regerar
          APPEND wa_zfictrmu TO it_zfictrmu.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Novos documentos Fornecedor
    SORT it_zfisolctr BY belnrf.
    LOOP AT it_bsik_aux INTO wa_bsik_aux.

      READ TABLE it_zfictrmu_atual INTO wa_zfictrmu_atual WITH KEY zvinc_cia = wa_bsik_aux-belnr BINARY SEARCH. "gerou docto ZGL, não pega novamente
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR wa_zfictrmu.

      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_bsik_aux-bukrs.
      READ TABLE it_zfisolctr INTO wa_zfisolctr WITH KEY belnrf = wa_bsik_aux-belnr BINARY SEARCH.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsik_aux-lifnr.

      wa_zfictrmu-zid_contr       = p_ctr.
      wa_zfictrmu-zid_empresa     = wa_bsik_aux-bukrs.
      wa_zfictrmu-zresdt_calc     = p_data.
      wa_zfictrmu-zvinc_cia       = wa_bsik_aux-belnr.
      wa_zfictrmu-buzei           = wa_bsik_aux-buzei.
      wa_zfictrmu-gjahr           = wa_bsik_aux-gjahr.
      wa_zfictrmu-nro_sol         = wa_zfisolctr-nro_sol.
      IF wa_bsik_aux-shkzg = 'H'.
        wa_zfictrmu-zvlor_princ     = wa_bsik_aux-wrbtr * -1.
      ELSE.
        wa_zfictrmu-zvlor_princ     = wa_bsik_aux-wrbtr.
      ENDIF.
      wa_zfictrmu-zmoeda          = wa_bsik_aux-pswsl.
      wa_zfictrmu-lifnr           = wa_bsik_aux-lifnr.

      wa_zfictrmu-usnam        = sy-uname.
      wa_zfictrmu-dt_entrada   = sy-datum.
      wa_zfictrmu-hr_entrada   = sy-uzeit.

      MOVE-CORRESPONDING wa_zfictrmu TO wa_saida.
      wa_saida-budat           = wa_bsik_aux-budat.
      wa_saida-gjahr           = wa_bsik_aux-gjahr.
      wa_saida-taxa            = wa_zfitaxctr-taxa.
      wa_saida-des_empresa     = wa_t001-butxt.
      wa_saida-zid_razao       = wa_zfitaxctr-hkont_f.
      wa_saida-zid_forcli      = wa_zfitaxctr-lifnr.

      CLEAR: wa_zfictrmu-zdata_mutuo, wa_saida-line_color.
      IF wa_zfisolctr-cd_mod NE 'R'.
        CLEAR wa_bseg.
        READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsik_aux-bukrs
                                                 belnr = wa_bsik_aux-belnr
                                                 gjahr = wa_bsik_aux-gjahr BINARY SEARCH.
        IF wa_bseg-augbl IS INITIAL.
          wa_saida-line_color      = 'C611'.
          wa_zfictrmu-zdata_mutuo  = wa_bsik_aux-budat.
        ELSE.
          wa_zfictrmu-zdata_mutuo     = wa_bseg-augdt.
        ENDIF.
      ELSE.
        wa_zfictrmu-zdata_mutuo     = wa_bsik_aux-budat.
      ENDIF.
      wa_saida-zdata_mutuo = wa_zfictrmu-zdata_mutuo.
      IF wa_zfitaxctr-waers NE 'BRL'. "Juros simples
        IF wa_zfisolctr-cd_mod = 'J'.
          wa_saida-moda = 'J'.
        ELSE.
          wa_saida-moda = 'P'.
        ENDIF.
      ENDIF.
      APPEND wa_saida    TO it_saida.

      READ TABLE it_zfictrmu INTO wa_zfictrmu WITH KEY zvinc_cia = wa_bsik_aux-belnr BINARY SEARCH.
      IF sy-subrc NE 0.
        IF  wa_zfictrmu-doc_lcto IS INITIAL. "Se não gerou lançamento contabil pode regerar
          APPEND wa_zfictrmu TO it_zfictrmu.
        ENDIF.
      ENDIF.

    ENDLOOP.

    "documentos já criados
    SORT it_zfisolctr BY nro_sol.
    LOOP AT it_zfictrmu_atual INTO wa_zfictrmu_atual.
      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfictrmu_atual-zid_empresa.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zfictrmu_atual-lifnr.
      READ TABLE it_zfisolctr INTO wa_zfisolctr WITH KEY nro_sol =  wa_zfictrmu_atual-nro_sol BINARY SEARCH.

      MOVE-CORRESPONDING wa_zfictrmu_atual TO wa_saida.
*      WA_SAIDA-GJAHR           = P_DATA+0(4).
      wa_saida-taxa            = wa_zfitaxctr-taxa.
      wa_saida-des_empresa     = wa_t001-butxt.
      IF wa_zfictrmu-lifnr IS NOT INITIAL.
        wa_saida-zid_razao       = wa_zfitaxctr-hkont_f.
        wa_saida-zid_forcli      = wa_zfitaxctr-lifnr.
      ELSE.
        wa_saida-zid_razao       = wa_zfitaxctr-hkont_c.
        wa_saida-zid_forcli      = wa_zfitaxctr-kunnr.
      ENDIF.
      IF wa_zfitaxctr-waers NE 'BRL'. "Juros simples
        IF wa_zfisolctr-cd_mod = 'J'.
          wa_saida-moda = 'J'.
        ELSE.
          wa_saida-moda = 'P'.
        ENDIF.
      ENDIF.
      APPEND wa_saida    TO it_saida.

      APPEND wa_zfictrmu_atual TO it_zfictrmu.
    ENDLOOP.
*    "Grava tabela
    CHECK it_zfictrmu[] IS NOT INITIAL.

*    MODIFY ZFICTRMU FROM TABLE IT_ZFICTRMU. "GRAVAR SOMENTE APÓS GERAR CONTABIL

  ENDMETHOD.                    "INSERIR_NOVO_MUTUO.

*=============================================================================*
*TABELAS
*=============================================================================*
  METHOD montar_alv.

    SELECT SINGLE *
       FROM zfitaxctr
       INTO wa_zfitaxctr
       WHERE zid_contr EQ p_ctr.

    FREE: it_fcat    , wa_fcat    ,
          it_fcat_02 , wa_fcat_02 ,
          it_fcat_03 , wa_fcat_03 ,
          it_fcat_04 , wa_fcat_04 .
*
    CLEAR vg_i .
    REFRESH it_sort.
    DEFINE m_sort.
      VG_I = VG_I + 1.
      WA_SORT-SPOS      = VG_I.
      WA_SORT-FIELDNAME = &1.
      wa_SORT-GROUP     = &2.
      IF &3 = 'D'.
        wa_SORT-DOWN        = 'X'.
      ELSE.
        wa_SORT-UP          = &3.
      ENDIF.
      wa_SORT-SUBTOT    = &4.
      APPEND WA_sort to IT_SORT.
      clear wa_SORT.
    END-OF-DEFINITION.

    CASE  x_screen.
      WHEN '0300'.

        DEFINE alv.
          WA_FCAT-HOTSPOT   = &1.
          WA_FCAT-TABNAME   = &2.
          WA_FCAT-FIELDNAME = &3.
          WA_FCAT-SCRTEXT_L = &4.
          WA_FCAT-NO_ZERO   = &5.
          WA_FCAT-OUTPUTLEN = &6.
          WA_FCAT-EDIT      = &7.
          WA_FCAT-DO_SUM    = &8.
          APPEND WA_FCAT TO IT_FCAT.
          CLEAR WA_FCAT.
        END-OF-DEFINITION.

        CLEAR vg_i.
        IF wa_zfitaxctr-waers = 'BRL'.
          m_sort:
                        'ZID_EMPRESA'     '' 'X' 'X',
                        'DES_EMPRESA'     '' 'X' ' ',
                        'ZID_CONTR'       '' 'X' ' ',
                        'ZID_RAZAO'       '' 'X' ' '.

          alv:
            ' '  'IT_SAIDA' 'ZID_EMPRESA              '         'Empresa                      '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'DES_EMPRESA              '         'Descrição                    '               ' '  '30'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_CONTR                '         'ID Contratos                 '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_RAZAO                '         'Conta Razão                  '               'X'  '15'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'TAXA                     '         'Taxa do CDI                  '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_FORCLI               '         'Fornecedor/Cliente           '               'X'  '13'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'NRO_SOL                  '         'Solicitação                  '               ' '  '15'    ' '  ' ' ,
            'X'  'IT_SAIDA' 'ZVINC_CIA                '         'Doc.Contabil                 '               'X'  '15'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'BUDAT                    '         'Data Docto.                  '               ' '  '12'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZDATA_MUTUO              '         'Data Comp.                   '               ' '  '12'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZVLOR_PRINC              '         'Val principal                '               ' '  '20'    ' '  'X' ,
            ' '  'IT_SAIDA' 'ZMOEDA                   '         'Moeda do contrato            '               ' '  '15'    ' '  ' ' .
        ELSE.
          m_sort:
                       'ZID_EMPRESA'     '' 'X' 'X',
                       'DES_EMPRESA'     '' 'X' ' ',
                       'ZID_CONTR'       '' 'X' ' ',
                       'ZID_RAZAO'       '' 'X' ' ',
                       'MODA'            '' 'X' 'X'.

          alv:
            ' '  'IT_SAIDA' 'ZID_EMPRESA              '         'Empresa                      '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'DES_EMPRESA              '         'Descrição                    '               ' '  '30'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_CONTR                '         'ID Contratos                 '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_RAZAO                '         'Conta Razão                  '               'X'  '15'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'MODA                     '         'Tipo                         '               'X'  '06'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'TAXA                     '         'Taxa do CDI                  '               ' '  '10'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZID_FORCLI               '         'Fornecedor/Cliente           '               'X'  '13'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'NRO_SOL                  '         'Solicitação                  '               ' '  '15'    ' '  ' ' ,
            'X'  'IT_SAIDA' 'ZVINC_CIA                '         'Doc.Contabil                 '               'X'  '15'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'BUDAT                    '         'Data Docto.                  '               ' '  '12'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZDATA_MUTUO              '         'Data Comp.                   '               ' '  '12'    ' '  ' ' ,
            ' '  'IT_SAIDA' 'ZVLOR_PRINC              '         'Val principal                '               ' '  '20'    ' '  'X' ,
            ' '  'IT_SAIDA' 'ZMOEDA                   '         'Moeda do contrato            '               ' '  '15'    ' '  ' ' .
        ENDIF.
      WHEN '0400'.
        DEFINE alv_04.
          WA_FCAT_04-FIELDNAME = &1.
          WA_FCAT_04-SCRTEXT_L = &2.
          WA_FCAT_04-SCRTEXT_M = &2.
          WA_FCAT_04-OUTPUTLEN = &3.
          WA_FCAT_04-DO_SUM    = &4.
          WA_FCAT_04-HOTSPOT   = &5.

          APPEND WA_FCAT_04 TO IT_FCAT_04.
          CLEAR WA_FCAT_04.
        END-OF-DEFINITION.
        m_sort:
                      'ZID_EMPRESA'     '' 'X' 'X',
                      'DES_EMPRESA'     '' 'X' ' '.

        IF wa_zfitaxctr-waers = 'BRL'.
          alv_04:
                         'ZID_EMPRESA      '         ' Empresa              '               '07'  ' ' ' ',          "Controle
                         'DES_EMPRESA      '         ' Descrição            '               '30'  ' ' ' ',          "Controle
                         'ZID_CONTR        '         ' Contrato             '               '10'  ' ' ' ',           "Controle
                         'ZRESDT_CALC      '         ' Dta Calculo          '               '15'  ' ' ' ',           "Data do Cálculo
                         'ZRESVL_PRINC     '         ' Valor princ          '               '20'  'X' ' ',           "Valor princ / Resid
                         'ZRESID_JUROS     '         ' Juros (+)            '               '20'  'X' ' ',           "Ingresso de juros
                         'ZRESID_IOF       '         ' IOF (+)              '               '20'  'X' ' ',           "Ingresso do IOF
                         'ZRESID_IR        '         ' IR (-)               '               '20'  'X' ' ',           "Amortização do IR
                         'ZRESID_RESULT    '         ' Valor Total          '               '20'  'X' ' ',           "Valor total
                         'DOC_LCTO         '         ' Doc. ZGL             '               '13'  ' ' 'X',           " documento ZGL
                         'BELNR            '         ' Doc.Ctb.Calc         '               '13'  ' ' 'X',           " documento contabil ir+iof+juros
                         'BELNR_RES        '         ' Doc.Ctb.Res          '               '13'  ' ' 'X'.           " documento contabil residual
        ELSE.
          alv_04:
                                  'ZID_EMPRESA      '         ' Empresa              '               '07'  ' ' ' ',          "Controle
                                  'DES_EMPRESA      '         ' Descrição            '               '30'  ' ' ' ',          "Controle
                                  'ZID_CONTR        '         ' Contrato             '               '10'  ' ' ' ',           "Controle
                                  'ZRESDT_CALC      '         ' Dta Calculo          '               '15'  ' ' ' ',           "Data do Cálculo
                                  'ZRESVL_PRINC     '         ' Valor princ          '               '20'  'X' ' ',           "Valor princ / Resid
                                  'ZRESID_JUROS     '         ' Juros (+)            '               '20'  'X' ' ',           "Ingresso de juros
                                  'ZRESID_IOF       '         ' IOF (+)              '               '20'  'X' ' ',           "Ingresso do IOF
                                  'ZRESID_IR        '         ' IR (-)               '               '20'  'X' ' ',           "Amortização do IR
                                  'DOC_LCTO         '         ' Doc. ZGL             '               '13'  ' ' 'X',           " documento ZGL
                                  'BELNR            '         ' Doc.Ctb.Calc         '               '13'  ' ' 'X',           " documento contabil ir+iof+juros
                                  'BELNR_RES        '         ' Doc.Ctb.Res          '               '13'  ' ' 'X',           " documento contabil residual
                                  'ZRESID_RESULT    '         ' Residual Principal   '               '20'  'X' ' ',           "Principal
                                  'BELNR_RESJ       '         ' Doc.Ctb.Res Juros    '               '13'  ' ' 'X',           " documento contabil residual
                                  'ZRESID_RESULTJ   '         ' Residual Juros       '               '20'  'X' ' '.           "Juros
        ENDIF.
      WHEN '0500'.

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = 'ZFICALCMU'
          CHANGING
            ct_fieldcat            = it_fcat_05
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

    ENDCASE.

  ENDMETHOD.                    "MONTAR_ALV
*=============================================================================*
*TABELAS
*=============================================================================*
  METHOD montar_lay.

    CLEAR: wa_layout, wa_variante.

    wa_layout-zebra      = abap_true.
    wa_layout-no_rowins  = abap_true.
    wa_layout-stylefname = 'ESTILO'.
    wa_layout-info_fname = 'LINE_COLOR'.
    wa_layout-sel_mode   = 'C'.
    wa_stable-row        = abap_true.
    wa_variante-report  = sy-repid.

  ENDMETHOD.                    "MONTAR_LAY
*=============================================================================*
*TABELAS
*=============================================================================*
  METHOD cria_alv.
    DATA: p_text         TYPE sdydo_text_element.
    CASE  x_screen.
      WHEN '0300'.
        IF wa_alv IS NOT INITIAL.
          CALL METHOD wa_alv->free.
          IF wa_cont IS NOT INITIAL.
            CALL METHOD wa_cont->free.
          ENDIF.
          FREE: wa_alv,wa_cont, obg_events.
        ENDIF.
        IF wa_cont IS INITIAL.

          CREATE OBJECT wa_cont
            EXPORTING
              container_name = 'TELA_0300'.

          CREATE OBJECT wa_alv
            EXPORTING
              i_shellstyle    = 0
              i_parent        = wa_cont
              i_appl_events   = space
              i_fcat_complete = space.


          CREATE OBJECT obg_events
            EXPORTING
              io_alv_grid = wa_alv.

          SET HANDLER:
                 obg_events->on_click    FOR wa_alv.

          wa_layout-grid_title = 'Solicitações'.

          CALL METHOD wa_alv->set_table_for_first_display
            EXPORTING
              is_layout       = wa_layout
              is_variant      = wa_variante
              i_save          = 'X'
            CHANGING
              it_outtab       = it_saida
              it_sort         = it_sort
              it_fieldcatalog = it_fcat.

          CALL METHOD wa_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
          CALL METHOD wa_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).

        ELSE.
          CALL METHOD wa_alv->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = it_fcat[].

          CALL METHOD wa_alv->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ENDIF.
      WHEN '0400'.
        IF wa_cont_04 IS INITIAL.

          CREATE OBJECT wa_cont_04
            EXPORTING
              container_name = 'C04'.

          wa_layout-grid_title = 'Calculos/Contabilizações'.


          CREATE OBJECT wa_alv_04
            EXPORTING
              i_shellstyle    = 0
              i_parent        = wa_cont_04
              i_appl_events   = space
              i_fcat_complete = space.

          CREATE OBJECT obg_events
            EXPORTING
              io_alv_grid = wa_alv_04.

          SET HANDLER:
                 obg_events->on_click    FOR wa_alv_04.

          CALL METHOD wa_alv_04->set_table_for_first_display
            EXPORTING
              is_layout       = wa_layout
              is_variant      = wa_variante
              i_save          = 'X'
            CHANGING
              it_outtab       = it_saida_calculo
              it_sort         = it_sort
              it_fieldcatalog = it_fcat_04.

        ELSE.
          CALL METHOD wa_alv_04->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.
      WHEN '0500'.
        IF wa_cont_05 IS INITIAL.

          CREATE OBJECT wa_cont_05
            EXPORTING
              container_name = 'C05'.

          wa_layout-grid_title = 'Memória de Cálculo'.


          CREATE OBJECT wa_alv_05
            EXPORTING
              i_shellstyle    = 0
              i_parent        = wa_cont_05
              i_appl_events   = space
              i_fcat_complete = space.

          CREATE OBJECT obg_events
            EXPORTING
              io_alv_grid = wa_alv_05.
*
*          SET HANDLER:
*                 OBG_EVENTS->ON_CLICK    FOR WA_ALV_05.

          CALL METHOD wa_alv_05->set_table_for_first_display
            EXPORTING
              is_layout       = wa_layout
              is_variant      = wa_variante
              i_save          = 'X'
            CHANGING
              it_outtab       = it_zficalcmu
              it_sort         = it_sort
              it_fieldcatalog = it_fcat_05.

        ELSE.
          CALL METHOD wa_alv_05->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "CRIA_ALV
*=============================================================================*
*MUTUOS_GERADO
*=============================================================================*
  METHOD mutuos_gerado.


  ENDMETHOD.                    "MUTUOS_GERADO
*=============================================================================*
*TABELAS
*=============================================================================*
  METHOD refresh_dados.

  ENDMETHOD.                    "REFRESH_DADOS

  METHOD gera_lote.

  ENDMETHOD.                    "GERA_LOTE


ENDCLASS.                    "ZCL_CONTROL_NDF IMPLEMENTATION

START-OF-SELECTION.
  CALL SCREEN 0100.

END-OF-SELECTION.

MODULE pbo OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  IF p_ctr IS NOT INITIAL.
    SELECT SINGLE *
    FROM zfitaxctr
    INTO wa_zfitaxctr
    WHERE zid_contr EQ p_ctr.
    IF wa_zfitaxctr-waers EQ 'BRL' .
      APPEND 'RESJ'  TO fcode.
      APPEND 'TRANS' TO fcode.
      APPEND 'TRANE' TO fcode.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'PF0100'  EXCLUDING fcode.
  SET TITLEBAR 'TI0100'.

  CREATE OBJECT: obg_control.

  obg_control->montar_alv( ).
  obg_control->montar_lay( ).
  obg_control->cria_alv( ).


ENDMODULE.                 " PBO  OUTPUT
*=============================================================================*
*Module  PAI  INPUT
*=============================================================================*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      FREE: it_saida, it_saida_calculo.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      IF  x_screen = '0500'.
        x_screen = '0400'.
      ELSEIF x_screen = '0400'.
        x_screen = '0300'.
      ELSE.
        FREE: it_saida, it_saida_calculo.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'EXE'.
      x_screen = '0300'.
      obg_control->seleciona_dados( ).
      obg_control->inserir_novo_mutuo( ).
    WHEN 'CALC'.
      x_screen = '0400'.
      PERFORM f_calculo.
    WHEN 'CTB'.
      x_screen = '0400'. "atualiza docto lançamento
      PERFORM f_contabil.
    WHEN 'RES'.
      x_screen = '0400'. " residual
      PERFORM f_residual USING 'P' .
    WHEN 'RESJ'.
      x_screen = '0400'. " residual Juros
      PERFORM f_residual USING 'J' .
    WHEN 'TRANS'.
      x_screen = '0400'.
      PERFORM f_transportar USING 'T'.
    WHEN 'TRANE'.
      x_screen = '0400'.
      PERFORM f_transportar USING 'E'.
    WHEN 'MEM'.
      x_screen = '0500'. " memória de calculo
      PERFORM f_memoria.
    WHEN 'FB08'.
      x_screen = '0400'. "
      PERFORM f_estorno.
    WHEN 'FBRA'.
      x_screen = '0400'. "
      PERFORM f_estorno_c.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_calculo .
  DATA: vobj_key     TYPE zib_contabil-obj_key,
        wa_bkpf      TYPE bkpf,
        vl_number    TYPE i,
        v_data       TYPE sy-datum,
        wl_zfitaxctr TYPE zfitaxctr,
        wl_zfisolctr TYPE zfisolctr.

  IF  it_zfictrmu[] IS INITIAL.
    MESSAGE 'Não há solicitações para cálculo' TYPE 'I'.
    EXIT.
  ENDIF.
  REFRESH: it_saida_calculo, it_zfirescmu.
  CLEAR: wa_saida_calculo.

  SELECT *
    FROM zfirescmu
    INTO TABLE it_zfirescmu
    FOR ALL ENTRIES IN it_zfictrmu
    WHERE zid_contr    = it_zfictrmu-zid_contr
    AND   zresdt_calc  = it_zfictrmu-zresdt_calc.

  IF it_zfirescmu[] IS INITIAL.
    PERFORM:
             f_inserir                      ,
             f_valor_principal_com_juros    ,
             f_sumario.
  ELSE.
    SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.
    LOOP AT it_zfirescmu INTO wa_zfirescmu.
      MOVE-CORRESPONDING wa_zfirescmu TO wa_saida_calculo.
      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfirescmu-zid_empresa.
      wa_saida_calculo-zid_empresa = wa_zfirescmu-zid_empresa.
      wa_saida_calculo-des_empresa =  wa_t001-butxt.
      IF wa_zfirescmu-doc_lcto IS NOT INITIAL.
        CONCATENATE 'ZGL17' wa_zfirescmu-doc_lcto p_data+0(4) INTO vobj_key.
        SELECT SINGLE *
         FROM zib_contabil_chv
         INTO  wa_zib_contabil_chv
         WHERE obj_key EQ vobj_key.

        IF sy-subrc = 0.
          wa_saida_calculo-belnr = wa_zib_contabil_chv-belnr.
          "
          "Grava se for juros simples WAERS NE 'BRL'
          IF wl_zfitaxctr-waers NE 'BRL'. "ALRS
            "Gravar na solicitacao
            v_data = p_data + 1. "mes seguinte
            IF wl_zfitaxctr-bukrs_f =  wa_saida_calculo-zid_empresa.
              SELECT SINGLE *
                FROM zfisolctr
                INTO wl_zfisolctr
                WHERE bukrs_f  = wa_zib_contabil_chv-bukrs
                AND   belnrf   = wa_zib_contabil_chv-belnr
                AND   gjahrf   = wa_zib_contabil_chv-gjahr
                AND   dt_lcto  = v_data.
            ELSE.
              SELECT SINGLE *
                FROM zfisolctr
                INTO wl_zfisolctr
                WHERE bukrs_c  = wa_zib_contabil_chv-bukrs
                AND   belnrc   = wa_zib_contabil_chv-belnr
                AND   gjahrc   = wa_zib_contabil_chv-gjahr
                AND   dt_lcto  = v_data.
            ENDIF.
            IF sy-subrc NE 0.
              v_data = p_data + 1. "mes seguinte
              CLEAR vl_number.
              CALL FUNCTION 'NUMBER_GET_NEXT'
                EXPORTING
                  nr_range_nr             = '01'
                  object                  = 'ZDOC_MUTU'
                IMPORTING
                  number                  = vl_number
                EXCEPTIONS
                  interval_not_found      = 1
                  number_range_not_intern = 2
                  object_not_found        = 3
                  quantity_is_0           = 4
                  quantity_is_not_1       = 5
                  interval_overflow       = 6
                  buffer_overflow         = 7
                  OTHERS                  = 8.

              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ELSE.
                wl_zfisolctr-nro_sol = vl_number.
              ENDIF.

              wl_zfisolctr-zid_contr          = wl_zfitaxctr-zid_contr.
              wl_zfisolctr-bukrs_f            = wl_zfitaxctr-bukrs_f.
              wl_zfisolctr-cd_mod             = 'J'. "Transporte do documento de juros
              wl_zfisolctr-dt_lcto            = v_data.
              wl_zfisolctr-dt_vct             = v_data.
              wl_zfisolctr-waers              = wl_zfitaxctr-waers.
              wl_zfisolctr-vlr_moeda_doc      = 0.
              wl_zfisolctr-hkont_f            = wl_zfitaxctr-hkont_f.
              wl_zfisolctr-hkont_c            = wl_zfitaxctr-hkont_c.
              IF wl_zfitaxctr-bukrs_f = wa_saida-zid_empresa.
                wl_zfisolctr-belnrf             = wa_zib_contabil_chv-belnr.
                wl_zfisolctr-gjahrf             = wa_zib_contabil_chv-gjahr.
                IF wl_zfitaxctr-bukrs_c = '9999'.
                  wl_zfisolctr-bukrs_c = '9999'.
                ENDIF.
              ELSE.
                wl_zfisolctr-bukrs_c            = wa_zib_contabil_chv-bukrs.
                wl_zfisolctr-belnrc             = wa_zib_contabil_chv-belnr.
                wl_zfisolctr-gjahrc             = wa_zib_contabil_chv-gjahr.
                IF wl_zfitaxctr-bukrs_f = '9999'.
                  wl_zfisolctr-bukrs_f = '9999'.
                ENDIF.
              ENDIF.
              MOVE : sy-mandt                    TO wl_zfisolctr-mandt,
                     sy-uname                    TO wl_zfisolctr-usnam,
                     sy-datum                    TO wl_zfisolctr-dt_entrada,
                     sy-uzeit                    TO wl_zfisolctr-hr_entrada.
              MODIFY zfisolctr FROM wl_zfisolctr.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND wa_saida_calculo TO it_saida_calculo.
    ENDLOOP.
  ENDIF.

ENDFORM.

FORM f_inserir.
  DATA: data_ini           TYPE zficalenmu-zcaldata,
        data_fim           TYPE zficalenmu-zcaldata,
        data_cal           TYPE zficalenmu-zcaldata,
        vg_last_day_aux(8),
        wl_zfitaxctr       TYPE zfitaxctr,
        wl_zfisolctr       TYPE zfisolctr.

  CONCATENATE p_data+0(6) '01' INTO data_ini.
  data_cal = data_ini - 10.
  data_fim = p_data.

  REFRESH: it_zficalcmu, it_zficalenmu.

  "CDI XRT
  SELECT *
   FROM zfit0071
   INTO TABLE it_zfit0071
   WHERE data BETWEEN data_cal AND data_fim.

  SORT it_zfit0071 BY data.


  PERFORM calc_dias_uteis USING data_cal data_fim.

  data_cal = data_ini.
  WHILE data_cal LE data_fim.
    wa_zficalenmu-zcaldata    = data_cal.

    READ TABLE holiday WITH KEY date = data_cal.
    IF sy-subrc = 0.
      wa_zficalenmu-zcalutil    = 'N'.
    ELSE.
      wa_zficalenmu-zcalutil    = 'S'.
    ENDIF.
    wa_zficalenmu-zcalferiado = ''.
    APPEND wa_zficalenmu TO it_zficalenmu.
    ADD 1 TO data_cal.
  ENDWHILE.
  SORT  it_zficalenmu BY zcaldata.


  CONCATENATE p_data+0(6) '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  "Apagar calculo anterior para contrato no mês
  LOOP AT it_zfictrmu INTO wa_zfictrmu.
    IF wa_zfictrmu-doc_lcto IS INITIAL.
      DELETE FROM zficalcmu WHERE zid_contr    = wa_zfictrmu-zid_contr
                            AND   data_calcmut BETWEEN data_ini AND vg_last_day.
      COMMIT WORK.
      EXIT.
    ENDIF.
  ENDLOOP.

  "INSERIR_DIAS
  CLEAR: wa_zficalcmu.
  SORT it_zfisolctr BY nro_sol.
  SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.
  LOOP AT it_zfictrmu INTO wa_zfictrmu.
    IF wa_zfictrmu-doc_lcto IS NOT INITIAL OR wa_zfictrmu-zdata_mutuo IS INITIAL.
      CONTINUE.
    ENDIF.
    READ TABLE it_zfisolctr INTO wa_zfisolctr WITH KEY nro_sol =  wa_zfictrmu-nro_sol BINARY SEARCH.
    IF wl_zfitaxctr-waers NE 'BRL'. "Juros simples
      IF wa_zfisolctr-cd_mod = 'J'. " Não calcula sobre juros
        CONTINUE.
      ENDIF.
    ENDIF.

    LOOP AT it_zficalenmu INTO wa_zficalenmu.
      wa_zficalcmu-data_calcmut = wa_zficalenmu-zcaldata.
      IF wa_zfictrmu-zdata_mutuo GT wa_zficalcmu-data_calcmut.
        CONTINUE.
      ENDIF.
      wa_zficalcmu-zid_contr   = wa_zfictrmu-zid_contr.
      wa_zficalcmu-zid_empresa = wa_zfictrmu-zid_empresa.
      wa_zficalcmu-zvinc_cia   = wa_zfictrmu-zvinc_cia.

      APPEND wa_zficalcmu TO it_zficalcmu.
      CLEAR wa_zficalcmu.
    ENDLOOP.
  ENDLOOP.


ENDFORM.

FORM f_valor_principal_com_juros .
  DATA: var_princ_acum TYPE zfictrmu-zvlor_princ,
        var_juros_acum TYPE zficalcmu-juros_cacl,
        wl_zfisolctr   TYPE zfisolctr,
        wl_zfitaxctr   TYPE zfitaxctr,
        wl_lfa1        TYPE lfa1,
        wl_kna1        TYPE kna1,
        var_data       TYPE zficalcmu-data_calcmut,
        var_cdi(1),
        var_iofp(1),
        var_calc(1),
        xachou(1),
        var_seq        TYPE i.
  DATA: var_a TYPE zficalcmu-cdi_calc,
        var_b TYPE zficalcmu-fator_calc,
        var_c TYPE zficalcmu-fator_calc.

  DATA: vr_liqu  TYPE zfialirmu-ziraliqu VALUE '22.5'.

  "IOF
  SELECT SINGLE *
     FROM zfialiofmu
     INTO wa_zfialiofmu
     WHERE ziofdataini LE sy-datum
     AND   ziofdatafin GE sy-datum.

  SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.

  IF wl_zfitaxctr-ir_fixo IS INITIAL.
    DATA: dt1    TYPE  sy-datum,
          dt2    TYPE  sy-datum,
          dt_max TYPE sy-datum,
          dt_min TYPE sy-datum.

    DATA prazo_dias LIKE p0347-scrdd.

    dt1 = p_data.
    dt2 = wl_zfitaxctr-dt_ini.

    dt_min = COND #( WHEN dt1 < dt2 THEN dt1 ELSE dt2 ).
    dt_max = COND #( WHEN dt1 > dt2 THEN dt1 ELSE dt2 ).


    CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
      EXPORTING
        date1         = dt_max
        date2         = dt_min
        output_format = '02'
      IMPORTING
        days          = prazo_dias.

    "IR
    SELECT SINGLE *
      FROM zfialirmu
      INTO wa_zfialirmu
    WHERE  zirdataini   <= p_data     AND  zirdatafin >=  p_data
      AND  prazo_inicio <= prazo_dias AND prazo_fim >= prazo_dias .

    IF sy-subrc IS INITIAL.
      vr_liqu  =  wa_zfialirmu-ziraliqu.
    ENDIF.

  ENDIF.


  CLEAR var_cdi.
  LOOP AT it_zfictrmu  INTO wa_zfictrmu.
    var_princ_acum  = wa_zfictrmu-zvlor_princ.
    var_seq = 1.
    CLEAR: var_iofp.

    var_calc = 'S'.
    IF wl_zfitaxctr-bukrs_c = wa_zfictrmu-zid_empresa. "Docs cliente
      SELECT SINGLE *
        FROM kna1
        INTO wl_kna1
        WHERE kunnr = wl_zfitaxctr-kunnr.
      IF sy-subrc = 0.
        IF wl_kna1-stcd2 IS NOT INITIAL. "CPF sem iof
          var_calc = 'N'.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr = wl_zfitaxctr-lifnr.
      IF sy-subrc = 0.
        IF wl_lfa1-stcd2 IS NOT INITIAL. "CPF sem iof
          var_calc = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM zfisolctr INTO wl_zfisolctr WHERE nro_sol = wa_zfictrmu-nro_sol.
    IF wl_zfisolctr-cd_mod = 'J'.
      CONTINUE. " Não calcula em cima de juros
    ENDIF.

    LOOP AT it_zficalcmu INTO wa_zficalcmu WHERE zid_empresa = wa_zfictrmu-zid_empresa
                                           AND   zvinc_cia   = wa_zfictrmu-zvinc_cia.

      CLEAR wa_zficalcmu-juros_cacl.
      IF wa_zfitaxctr-waers NE 'BRL'.
*        WA_ZFICALCMU-JUROS_CACL = ( WA_ZFITAXCTR-TAXA / 36000 ) * WA_ZFICTRMU-ZVLOR_PRINC.
        wa_zficalcmu-juros_cacl = ( wa_zfitaxctr-taxa / wa_zfitaxctr-qdt_dias ) * wa_zfictrmu-zvlor_princ.


        "**********************************************************************************
      ELSEIF wa_zfitaxctr-jros_cdi_tx = 'X'.
        var_a = ( ( wa_zfitaxctr-taxa ) / 21 ).
        var_b = ( ( ( 1 +  ( wa_zfitaxctr-taxa / 100 ) ) ** var_a ) - 1 ).
        wa_zficalcmu-fator_calc = var_b.
        wa_zficalcmu-fator_conv = (  var_b ).
        CLEAR: wa_zfit0071, wa_zficalenmu.
        READ TABLE it_zficalenmu INTO wa_zficalenmu WITH KEY zcaldata = wa_zficalcmu-data_calcmut BINARY SEARCH.
        IF wa_zficalenmu-zcalutil    = 'S'.
          IF var_seq GT 1 OR wl_zfisolctr-cd_mod EQ 'R'. "calcula juros para residual no 1 dia
            DATA(xaliqcdi) =  wa_zficalcmu-fator_conv.

*            WA_ZFICALCMU-JUROS_CACL = VAR_PRINC_ACUM * WA_ZFICALCMU-FATOR_CONV. " Juros diário

          ENDIF.
          ADD 1 TO var_seq.
        ENDIF.

        "*******
        "CDI
        CLEAR: wa_zfit0071, wa_zficalenmu.
        READ TABLE it_zficalenmu INTO wa_zficalenmu WITH KEY zcaldata = wa_zficalcmu-data_calcmut BINARY SEARCH.
        IF wa_zficalenmu-zcalutil    = 'S'.
          "busca ultimo dia util
          CLEAR xachou.
          var_data = wa_zficalcmu-data_calcmut.
          IF wa_zficalcmu-data_calcmut GT sy-datum.
            var_data = sy-datum.
          ENDIF.

          WHILE xachou = ''.
            SUBTRACT 1 FROM var_data.
            READ TABLE holiday WITH KEY date = var_data.
            IF sy-subrc NE 0. "util
              READ TABLE it_zfit0071 INTO wa_zfit0071 WITH KEY data = var_data BINARY SEARCH.
              IF sy-subrc NE 0.
                var_cdi = 'E'.
                MESSAGE i000(z01) WITH  'Taxa CDI (XRT) não encontrada' var_data.
                EXIT.
              ELSEIF wa_zfit0071-tx_cdi_diaria IS INITIAL.
                var_cdi = 'E'.
                MESSAGE i000(z01) WITH  'Taxa CDI (XRT) Zerada' var_data.
                EXIT.
              ENDIF.
              xachou = 'X'.
              "
              EXIT.
            ENDIF.
          ENDWHILE.
          IF var_cdi = 'E'.
            CLEAR wa_zfit0071.
            EXIT.
          ENDIF.
        ENDIF.
        "ALTERAÇÃO
        IF  wa_zfit0071-tx_cdi_diaria IS NOT INITIAL.
          wa_zfitaxcdimu-cdi_txano = ( ( ( ( wa_zfit0071-tx_cdi_diaria / 100 ) + 1 ) ** 252 ) - 1 ) * 100.
          wa_zficalcmu-perccdi_contr = wa_zfitaxctr-taxa.
          wa_zficalcmu-cdi_tx_ano = wa_zfitaxcdimu-cdi_txano.
          var_a = wa_zficalcmu-perccdi_contr / 100.
          var_c = 1 / 252.
          "WA_ZFICALCMU-CDI_CALC =  WA_ZFICALCMU-CDI_TX_ANO * VAR_A / 100.
          wa_zficalcmu-cdi_calc =  ( ( ( wa_zficalcmu-cdi_tx_ano / 100 ) + 1 ) ** var_c ) - 1.

          var_b = wa_zficalcmu-cdi_calc + 1.

          "WA_ZFICALCMU-FATOR_CALC = VAR_B ** VAR_C.
          wa_zficalcmu-fator_calc = var_b ** var_a.
          wa_zficalcmu-fator_conv = wa_zficalcmu-fator_calc - 1.

          IF wa_zficalenmu-zcalutil    = 'S'.
            IF var_seq GT 1 OR wl_zfisolctr-cd_mod EQ 'R'. "calcula juros para residual no 1 dia
              DATA(xcalcaliquota) =  wa_zficalcmu-fator_conv.
*              WA_ZFICALCMU-JUROS_CACL = VAR_PRINC_ACUM * WA_ZFICALCMU-FATOR_CONV. " Juros diário
            ENDIF.
            ADD 1 TO var_seq.
          ENDIF.
        ENDIF.


        "******************************Adicionado calculo CDI com duas taxas******
        wa_zficalcmu-juros_cacl = var_princ_acum * xcalcaliquota * xaliqcdi * ( wa_zfitaxctr-perc_cdi / 100 ). " Juros diário

      ELSEIF wa_zfitaxctr-jur_scdi = 'X'. "juros compostos sem CDI
        var_a = ( ( wa_zfitaxctr-taxa ) / 21 ).
        var_b = ( ( ( 1 +  ( wa_zfitaxctr-taxa / 100 ) ) ** var_a ) - 1 ).
        wa_zficalcmu-fator_calc = var_b.
        wa_zficalcmu-fator_conv = (  var_b ).
        CLEAR: wa_zfit0071, wa_zficalenmu.
        READ TABLE it_zficalenmu INTO wa_zficalenmu WITH KEY zcaldata = wa_zficalcmu-data_calcmut BINARY SEARCH.
        IF wa_zficalenmu-zcalutil    = 'S'.
          IF var_seq GT 1 OR wl_zfisolctr-cd_mod EQ 'R'. "calcula juros para residual no 1 dia
            wa_zficalcmu-juros_cacl = var_princ_acum * wa_zficalcmu-fator_conv. " Juros diário
          ENDIF.
          ADD 1 TO var_seq.
        ENDIF.
      ELSE.
        "CDI
        CLEAR: wa_zfit0071, wa_zficalenmu.
        READ TABLE it_zficalenmu INTO wa_zficalenmu WITH KEY zcaldata = wa_zficalcmu-data_calcmut BINARY SEARCH.
        IF wa_zficalenmu-zcalutil    = 'S'.
          "busca ultimo dia util
          CLEAR xachou.
          var_data = wa_zficalcmu-data_calcmut.
          IF wa_zficalcmu-data_calcmut GT sy-datum.
            var_data = sy-datum.
          ENDIF.

          WHILE xachou = ''.
            SUBTRACT 1 FROM var_data.
            READ TABLE holiday WITH KEY date = var_data.
            IF sy-subrc NE 0. "util
              READ TABLE it_zfit0071 INTO wa_zfit0071 WITH KEY data = var_data BINARY SEARCH.
              IF sy-subrc NE 0.
                var_cdi = 'E'.
                MESSAGE i000(z01) WITH  'Taxa CDI (XRT) não encontrada' var_data.
                EXIT.
              ELSEIF wa_zfit0071-tx_cdi_diaria IS INITIAL.
                var_cdi = 'E'.
                MESSAGE i000(z01) WITH  'Taxa CDI (XRT) Zerada' var_data.
                EXIT.
              ENDIF.
              xachou = 'X'.
              "
              EXIT.
            ENDIF.
          ENDWHILE.
          IF var_cdi = 'E'.
            CLEAR wa_zfit0071.
            EXIT.
          ENDIF.
        ENDIF.
        "ALTERAÇÃO
        IF  wa_zfit0071-tx_cdi_diaria IS NOT INITIAL.
          wa_zfitaxcdimu-cdi_txano = ( ( ( ( wa_zfit0071-tx_cdi_diaria / 100 ) + 1 ) ** 252 ) - 1 ) * 100.
          wa_zficalcmu-perccdi_contr = wa_zfitaxctr-taxa.
          wa_zficalcmu-cdi_tx_ano = wa_zfitaxcdimu-cdi_txano.
          var_a = wa_zficalcmu-perccdi_contr / 100.
          var_c = 1 / 252.
          "WA_ZFICALCMU-CDI_CALC =  WA_ZFICALCMU-CDI_TX_ANO * VAR_A / 100.
          wa_zficalcmu-cdi_calc =  ( ( ( wa_zficalcmu-cdi_tx_ano / 100 ) + 1 ) ** var_c ) - 1.

          var_b = wa_zficalcmu-cdi_calc + 1.

          "WA_ZFICALCMU-FATOR_CALC = VAR_B ** VAR_C.
          wa_zficalcmu-fator_calc = var_b ** var_a.
          wa_zficalcmu-fator_conv = wa_zficalcmu-fator_calc - 1.

          IF wa_zficalenmu-zcalutil    = 'S'.
            IF var_seq GT 1 OR wl_zfisolctr-cd_mod EQ 'R'. "calcula juros para residual no 1 dia
              wa_zficalcmu-juros_cacl = var_princ_acum * wa_zficalcmu-fator_conv. " Juros diário
            ENDIF.
            ADD 1 TO var_seq.
          ENDIF.
        ENDIF.
      ENDIF.

      wa_zficalcmu-princjuros_calc = var_princ_acum + wa_zficalcmu-juros_cacl. "Principal + juros
      var_princ_acum = wa_zficalcmu-princjuros_calc.

      IF wa_zfitaxctr-waers EQ 'BRL'.
        IF wa_zfitaxctr-sem_ir IS INITIAL.
          wa_zficalcmu-ir_diario = wa_zficalcmu-juros_cacl * vr_liqu / 100.
        ENDIF.
        IF wa_zfitaxctr-sem_iof IS INITIAL.
          IF var_calc = 'S'." AND WL_ZFISOLCTR-CD_MOD NE 'A'. " amortização não calcula IOF
            IF var_iofp IS INITIAL AND wl_zfisolctr-cd_mod EQ 'E'. "IOF CABEÇA / somente empréstimo
              var_iofp = 'X'.
              wa_zficalcmu-iof_diario = wa_zficalcmu-princjuros_calc * wa_zfialiofmu-ziofaliq_pri / 100.
              wa_zficalcmu-iof_diario = wa_zficalcmu-iof_diario + ( wa_zficalcmu-princjuros_calc * wa_zfialiofmu-ziofaliq / 100 ).
            ELSE.
              wa_zficalcmu-iof_diario = wa_zficalcmu-princjuros_calc * wa_zfialiofmu-ziofaliq / 100.
            ENDIF.
          ENDIF.
        ELSEIF wl_zfisolctr-cd_mod EQ 'E' AND var_iofp IS INITIAL.
          var_iofp = 'X'.
          wa_zficalcmu-iof_diario = wa_zficalcmu-princjuros_calc * 188 / 10000.
        ENDIF.
      ENDIF.


      "*************************************************************************************************

      wa_zficalcmu-usnam        = sy-uname.
      wa_zficalcmu-dt_entrada   = sy-datum.
      wa_zficalcmu-hr_entrada   = sy-uzeit.
      MODIFY it_zficalcmu FROM wa_zficalcmu.
      CLEAR wa_zficalcmu.
    ENDLOOP.

    CHECK it_zficalcmu[] IS NOT INITIAL.

    IF  var_cdi = 'E'.
      REFRESH it_zficalcmu.
      EXIT.
    ENDIF.
  ENDLOOP.

*    "Grava tabela
  IF  var_cdi NE 'E'.
    MODIFY zficalcmu FROM TABLE it_zficalcmu.
  ENDIF.

ENDFORM.                    " F_VALOR_PRINCIPAL_COM_JUROS

FORM calc_dias_uteis USING dodia atedia.
  DATA : hol_id(2)  TYPE c  VALUE 'ZF',        " holiday calender id
         fact_id(2) TYPE c  VALUE 'ZF',        " factory calender id
         fromdate   TYPE sydatum ,   " from date
         enddate    TYPE sydatum,    " end date
         v_days     TYPE i.


  DATA: v_yrfrm(4)  TYPE n,              "year valid from
        v_yrvlto(4) TYPE n.             "year valid to

  REFRESH holiday.

  fromdate   = dodia.
  enddate    = atedia.
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar           = hol_id
      factory_calendar           = fact_id
      date_from                  = fromdate
      date_to                    = enddate
    IMPORTING
      year_of_valid_from         = v_yrfrm
      year_of_valid_to           = v_yrvlto
*     RETURNCODE                 =
    TABLES
      holidays                   = holiday
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.


ENDFORM.                    "CALC_dias_uteis
*&---------------------------------------------------------------------*
*&      Form  F_AMORTIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sumario.
  REFRESH: it_saida_calculo,it_zfirescmu.
  DATA: v_juros      TYPE bseg-wrbtr,
        v_iof        TYPE bseg-wrbtr,
        v_ir         TYPE bseg-wrbtr,
        v_pri        TYPE bseg-wrbtr,
        wl_zfitaxctr TYPE zfitaxctr.

  SORT it_zfisolctr BY nro_sol.
  SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.
  LOOP AT it_zfictrmu INTO  wa_zfictrmu.
    CLEAR wa_zfirescmu.
    READ TABLE it_zfisolctr INTO wa_zfisolctr WITH KEY nro_sol =  wa_zfictrmu-nro_sol BINARY SEARCH.

    LOOP AT it_zficalcmu INTO   wa_zficalcmu WHERE zid_contr   = wa_zfictrmu-zid_contr
                                            AND   zid_empresa = wa_zfictrmu-zid_empresa
                                            AND   zvinc_cia   = wa_zfictrmu-zvinc_cia.
      ADD wa_zficalcmu-juros_cacl  TO wa_zfirescmu-zresid_juros.
      ADD wa_zficalcmu-iof_diario TO wa_zfirescmu-zresid_iof.
      ADD wa_zficalcmu-ir_diario  TO wa_zfirescmu-zresid_ir.
    ENDLOOP.
*    IF WA_ZFIRESCMU-ZRESID_JUROS = 0.
*      CONTINUE.
*    ENDIF.
    wa_zfirescmu-zid_contr    = wa_zfictrmu-zid_contr.
    wa_zfirescmu-zid_empresa  = wa_zfictrmu-zid_empresa.
    wa_zfirescmu-zresdt_calc  = p_data.
    wa_zfirescmu-zresvl_princ = wa_zfictrmu-zvlor_princ.

    "arredonda
    v_juros = wa_zfirescmu-zresid_juros.
    v_iof   = wa_zfirescmu-zresid_iof.
    v_ir    = wa_zfirescmu-zresid_ir.
    v_pri   = wa_zfirescmu-zresvl_princ.

    wa_zfirescmu-zresid_juros  = v_juros.
    wa_zfirescmu-zresid_iof    = v_iof.
    wa_zfirescmu-zresid_ir     = v_ir.
    wa_zfirescmu-zresvl_princ  = v_pri.
    wa_zfirescmu-zresid_result = v_pri + v_juros +  v_iof - v_ir.

    IF wa_zfitaxctr-waers NE 'BRL'. "Juros simples
      IF wa_zfisolctr-cd_mod = 'J'.
        wa_zfirescmu-zresid_resultj = wa_zfictrmu-zvlor_princ.
        wa_zfirescmu-zresid_result  = 0.
        wa_zfirescmu-zresid_juros   = 0.
        wa_zfirescmu-zresid_iof     = 0.
        wa_zfirescmu-zresid_ir      = 0.
        wa_zfirescmu-zresvl_princ   = 0.
      ELSE.
        wa_zfirescmu-zresid_result  = wa_zfictrmu-zvlor_princ.
        wa_zfirescmu-zresid_resultj = 0.
      ENDIF.
    ENDIF.
    "
    wa_zfirescmu-usnam        = sy-uname.
    wa_zfirescmu-dt_entrada   = sy-datum.
    wa_zfirescmu-hr_entrada   = sy-uzeit.
    "
    COLLECT  wa_zfirescmu INTO it_zfirescmu.
  ENDLOOP.

  LOOP AT it_zfirescmu INTO  wa_zfirescmu.
    MOVE-CORRESPONDING wa_zfirescmu TO wa_saida_calculo.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfirescmu-zid_empresa.
    wa_saida_calculo-zid_empresa = wa_zfirescmu-zid_empresa.
    wa_saida_calculo-des_empresa =  wa_t001-butxt.
    APPEND wa_saida_calculo TO it_saida_calculo.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_contabil.
  DATA: wl_zfitaxctr       TYPE zfitaxctr,
        wl_zglt031         TYPE zglt031,
        gt_zgl032_aux      TYPE TABLE OF zglt032,
        gt_zglt032         TYPE TABLE OF zglt032,
        wl_zglt032         TYPE zglt032,
        wl_zglt035         TYPE zglt035,
        gt_zglt036         TYPE TABLE OF zglt036,
        wl_zglt036         TYPE zglt036,
        wl_zfisolctr       TYPE zfisolctr,
        wl_tbsl            TYPE tbsl,
        v_bukrs            TYPE zglt035-bukrs,
        v_gsber            TYPE zglt036-gsber,
        v_tp_lcto          TYPE zglt031-tp_lcto,
        dp_resp            TYPE char2,
        descricao          TYPE zdescr_lote,
        v_namef            TYPE lfa1-name1,
        v_namec            TYPE kna1-name1,
        v_vbund            TYPE zglt036-vbund,
        v_vbund_f          TYPE zglt036-vbund,
        v_vbund_c          TYPE zglt036-vbund,
        tabix              TYPE sy-tabix,
        v_zid_contr(6),
        vg_last_day_aux(8),
        wl_erro(1).


  CONCATENATE p_data+0(6) '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  IF it_saida_calculo[] IS INITIAL.
    MESSAGE 'Gerar  cálculo, para contabilização.' TYPE 'I'.
    EXIT.
  ENDIF.

  IF p_data_i IS NOT INITIAL.
    MESSAGE 'Cálculo apenas para simulação, não gerar!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.

  IF p_data NE vg_last_day AND wa_zfitaxctr-dt_fim NE p_data.   " deixa contabilizar se for encerramento.
    MESSAGE 'Para gerar documento contábil, informe último dia, ou fim do contrato' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA: r_gerar_lote    TYPE REF TO zcl_gerar_lote.
  CREATE OBJECT r_gerar_lote.

  SELECT SINGLE vbund INTO v_vbund_f FROM lfa1 WHERE lifnr = wl_zfitaxctr-lifnr.
  SELECT SINGLE vbund INTO v_vbund_c FROM kna1 WHERE kunnr = wl_zfitaxctr-kunnr.

  CLEAR wl_erro.
  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    IF wa_saida_calculo-doc_lcto IS NOT INITIAL.
      MESSAGE s836(sd) WITH text-005.
      wl_erro = 'X'.
      EXIT.
    ENDIF.
    IF wl_zfitaxctr-bukrs_c = wa_saida_calculo-zid_empresa.
      v_bukrs   = wl_zfitaxctr-bukrs_c.
      v_gsber   = wl_zfitaxctr-gsber_c.
      v_tp_lcto = wl_zfitaxctr-tp_prov_c.
      v_vbund   = v_vbund_c.
    ELSE.
      v_bukrs   = wl_zfitaxctr-bukrs_f.
      v_gsber   = wl_zfitaxctr-gsber_f.
      v_tp_lcto = wl_zfitaxctr-tp_prov_f.
      v_vbund   = v_vbund_f.
    ENDIF.

    SELECT SINGLE * FROM zglt031 INTO wl_zglt031 WHERE tp_lcto EQ v_tp_lcto.
    IF sy-subrc NE 0.
      "Tipo de Lançamento não Encontrado!
      MESSAGE s836(sd) WITH text-004.
      wl_erro = 'X'.
      EXIT.
    ELSE.
      SELECT * FROM zglt032 INTO TABLE gt_zglt032 WHERE tp_lcto EQ v_tp_lcto.
      IF sy-subrc NE 0.
        MESSAGE s836(sd) WITH text-003.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK  wl_erro IS INITIAL.

  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    IF wa_saida_calculo-doc_lcto IS NOT INITIAL.
      MESSAGE s836(sd) WITH text-005.
      wl_erro = 'X'.
      EXIT.
    ENDIF.
    tabix = sy-tabix.

    IF wl_zfitaxctr-bukrs_c = wa_saida_calculo-zid_empresa.
      v_bukrs   = wl_zfitaxctr-bukrs_c.
      v_gsber   = wl_zfitaxctr-gsber_c.
      v_tp_lcto = wl_zfitaxctr-tp_prov_c.
      v_vbund   = v_vbund_c.
    ELSE.
      v_bukrs   = wl_zfitaxctr-bukrs_f.
      v_gsber   = wl_zfitaxctr-gsber_f.
      v_tp_lcto = wl_zfitaxctr-tp_prov_f.
      v_vbund   = v_vbund_f.
    ENDIF.

    SELECT SINGLE * FROM zglt031 INTO wl_zglt031 WHERE tp_lcto EQ v_tp_lcto.
    IF sy-subrc EQ 0.
      FREE: gt_zglt032.

      SELECT * FROM zglt032 INTO TABLE gt_zglt032 WHERE tp_lcto EQ v_tp_lcto.

      IF sy-subrc EQ 0.
        MOVE wl_zglt031-descricao TO descricao.
        MOVE wl_zglt031-dpto_resp TO dp_resp.

        " Gera número do lote
        CALL METHOD zcl_gerar_lote=>create_lote
          EXPORTING
            i_bukrs      = wa_saida_calculo-zid_empresa
            i_descr_lote = descricao
            i_dep_resp   = dp_resp
            i_user_resp  = sy-uname
*           I_STATUS_LOTE = 'L'
          IMPORTING
            e_num_lote   = wl_zglt035-lote.

        MOVE:
              v_bukrs                   TO wl_zglt035-bukrs,
              v_tp_lcto                 TO wl_zglt035-tp_lcto,
              dp_resp                   TO wl_zglt035-dpto_resp,
              wl_zfitaxctr-waers        TO wl_zglt035-moeda_doc,
              wl_zglt031-st_lc_moeda    TO wl_zglt035-st_lc_moeda,
              wl_zglt031-moeda_int_hist TO wl_zglt035-moeda_int_hist,
              wl_zglt031-moeda_ft_hist  TO wl_zglt035-moeda_ft_hist,
              wl_zglt031-moeda_gp_hist  TO wl_zglt035-moeda_gp_hist,
              wl_zglt031-blart          TO wl_zglt035-blart,
              wl_zglt031-descricao      TO wl_zglt035-xblnr,
              wl_zglt031-bktxt          TO wl_zglt035-bktxt,
              p_data                    TO wl_zglt035-budat,
              p_data                    TO wl_zglt035-bldat,
              p_data                    TO wl_zglt035-dt_lcto,
              wl_zglt031-prov_est       TO wl_zglt035-prov_est,
              wl_zglt031-st_ap_fiscal   TO wl_zglt035-st_ap_fiscal,
              p_data+4(2)               TO wl_zglt035-monat,
              p_data+0(4)               TO wl_zglt035-gjahr,
              sy-uname                  TO wl_zglt035-usnam,
              sy-datum                  TO wl_zglt035-dt_entrada,
              sy-uzeit                  TO wl_zglt035-hr_entrada.

        v_zid_contr = wa_saida_calculo-zid_contr.
        CONCATENATE 'CTR-' v_zid_contr INTO wl_zglt035-bktxt.

        FREE: gt_zglt036.
        CLEAR: v_namef, v_namec.
        LOOP AT gt_zglt032 INTO wl_zglt032.
          MOVE-CORRESPONDING wl_zglt032 TO wl_zglt036.
          MOVE: sy-tabix              TO wl_zglt036-seqitem,
                wl_zglt032-tp_lcto    TO wl_zglt036-tp_lcto,
                wl_zglt032-bschl      TO wl_zglt036-bschl,
                v_gsber               TO wl_zglt036-gsber,
                v_vbund               TO wl_zglt036-vbund,
                wl_zglt032-hkont      TO wl_zglt036-hkont,
                wl_zglt032-sgtxt      TO wl_zglt036-sgtxt,
                wl_zglt032-umskz      TO wl_zglt036-umskz.

          SELECT SINGLE *
             FROM tbsl
             INTO  wl_tbsl
             WHERE bschl EQ  wl_zglt032-bschl.
          IF wl_tbsl-koart = 'K'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_zfitaxctr-lifnr
              IMPORTING
                output = wl_zfitaxctr-lifnr.
            MOVE wl_zfitaxctr-lifnr TO wl_zglt036-hkont.
            SELECT SINGLE name1 INTO v_namef FROM lfa1 WHERE lifnr = wl_zfitaxctr-lifnr.
            CONCATENATE wl_zglt032-zuonr v_namef INTO wl_zglt036-sgtxt SEPARATED BY space.
          ELSEIF wl_tbsl-koart = 'D'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_zfitaxctr-kunnr
              IMPORTING
                output = wl_zfitaxctr-kunnr.
            MOVE wl_zfitaxctr-kunnr TO wl_zglt036-hkont.
            SELECT SINGLE name1 INTO v_namec FROM kna1 WHERE kunnr = wl_zfitaxctr-kunnr.
            CONCATENATE wl_zglt032-zuonr v_namec INTO wl_zglt036-sgtxt SEPARATED BY space.
          ENDIF.

          IF wl_zglt032-zuonr = 'JUROS'.
            IF wa_saida_calculo-zresid_juros LT 0.
              MULTIPLY wa_saida_calculo-zresid_juros BY -1.
            ENDIF.
            MOVE:  wa_saida_calculo-zresid_juros  TO wl_zglt036-vlr_moeda_doc.
          ELSEIF wl_zglt032-zuonr = 'IR'.
            IF wa_saida_calculo-zresid_ir LT 0.
              MULTIPLY wa_saida_calculo-zresid_ir BY -1.
            ENDIF.
            MOVE:  wa_saida_calculo-zresid_ir  TO wl_zglt036-vlr_moeda_doc.
          ELSEIF wl_zglt032-zuonr = 'IOF'.
            IF wa_saida_calculo-zresid_iof LT 0.
              MULTIPLY wa_saida_calculo-zresid_iof BY -1.
            ENDIF.
            MOVE:  wa_saida_calculo-zresid_iof  TO wl_zglt036-vlr_moeda_doc.
          ENDIF.


          APPEND wl_zglt036 TO gt_zglt036.

          CLEAR: wl_zglt036, wl_zglt032.

        ENDLOOP.

        CALL METHOD zcl_gerar_lote=>contabilizar_lote(
          CHANGING
            i_zglt036 = gt_zglt036
            i_zglt035 = wl_zglt035 ).

        MOVE:  wl_zglt035-doc_lcto TO wa_saida_calculo-doc_lcto.
        MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING doc_lcto.
        "
        READ TABLE it_zfirescmu INTO wa_zfirescmu WITH KEY zid_contr   = wa_saida_calculo-zid_contr
                                                           zid_empresa = wa_saida_calculo-zid_empresa.
        IF sy-subrc = 0.
          MOVE:  wl_zglt035-doc_lcto TO wa_zfirescmu-doc_lcto.
          MODIFY it_zfirescmu FROM wa_zfirescmu INDEX sy-tabix TRANSPORTING doc_lcto.

        ENDIF.

        DATA:  v_liberado    TYPE char01.

        CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
          EXPORTING
            p_num_lote = wl_zglt035-lote
          IMPORTING
            p_liberado = v_liberado.

      ELSE.
        MESSAGE s836(sd) WITH text-003.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
    ELSE.
      "Tipo de Lançamento não Encontrado!
      MESSAGE s836(sd) WITH text-004.
      wl_erro = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

  CHECK wl_erro IS INITIAL.

*    "Grava tabela
  CHECK it_zfirescmu[] IS NOT INITIAL.

  MODIFY zfirescmu FROM TABLE it_zfirescmu. "sumário de calculos

  MODIFY zfictrmu FROM TABLE it_zfictrmu.  " Grava Contrato e sumário com mesma data

  LOOP AT it_zfirescmu INTO wa_zfirescmu.
    IF wa_zfirescmu-doc_lcto IS NOT INITIAL.
      UPDATE zfictrmu SET doc_lcto = wa_zfirescmu-doc_lcto
      WHERE zid_contr     = wa_zfirescmu-zid_contr
      AND   zid_empresa   = wa_zfirescmu-zid_empresa
      AND   zresdt_calc   = wa_zfirescmu-zresdt_calc.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RESIDUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_residual USING p_tipo.
  DATA: wl_erro(1),
        wl_vlrc(16),
        wl_vlrn      TYPE p DECIMALS 2,
        v_blart      TYPE bkpf-blart,
        v_gsber      TYPE zglt036-gsber,
        v_name1      TYPE lfa1-name1,
        v_umskz      TYPE zfitaxctr-umskz_f,
        p_mode       LIKE rfpdo-allgazmd,
        v_data       TYPE sy-datum,
        msg_no       TYPE t100-msgnr,
        msg_text     TYPE string,
        tabix        TYPE sy-tabix,
        vl_qtde      TYPE i,
        vl_number    TYPE i,
        wl_tbsl      TYPE tbsl,
        wl_zfitaxctr TYPE zfitaxctr,
        wl_zfisolctr TYPE zfisolctr,
        vl_moeda_int TYPE zfictrmu-zvlor_princ,
        vl_moeda_acu TYPE zfictrmu-zvlor_princ,
        it_bseg      TYPE TABLE OF bseg,
        wa_bseg      TYPE bseg,
        it_bkpf      TYPE TABLE OF bkpf WITH HEADER LINE.
*

  IF it_saida_calculo[] IS INITIAL.
    MESSAGE 'Não foi gerado Contabilização' TYPE 'I'.
    EXIT.
  ENDIF.
  v_data = p_data + 1. "mes seguinte

  CLEAR wl_erro.
  LOOP AT it_saida INTO wa_saida.
    "
    SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = wa_saida-zid_contr.
    IF wl_zfitaxctr-waers = 'BRL'.
      CONTINUE.
    ENDIF.
    IF wl_zfitaxctr-bukrs_f = wa_saida-zid_empresa.
      SELECT SINGLE *
        FROM zfisolctr
        INTO wl_zfisolctr
        WHERE bukrs_f  = wl_zfitaxctr-bukrs_f
        AND   belnrf   = wa_saida-zvinc_cia
        AND   gjahrf   = wa_saida-gjahr
        AND   dt_lcto  = v_data
        AND   cd_mod   = p_tipo.
    ELSE.
      SELECT SINGLE *
        FROM zfisolctr
        INTO wl_zfisolctr
        WHERE bukrs_c  = wl_zfitaxctr-bukrs_c
        AND   belnrc   = wa_saida-zvinc_cia
        AND   gjahrc   = wa_saida-gjahr
        AND   dt_lcto  = v_data
        AND   cd_mod   = p_tipo.
    ENDIF.
    IF sy-subrc = 0.
      IF p_tipo = 'J'.
        MESSAGE 'Transporte juros já realizado' TYPE 'I'.
      ELSE.
        MESSAGE 'Transporte principal já realizado' TYPE 'I'.
      ENDIF.
      wl_erro = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.


  IF wl_erro = 'X'.
    EXIT.
  ENDIF.


  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    IF wa_saida_calculo-belnr IS INITIAL.
      IF wa_saida_calculo-zresid_result NE wa_saida_calculo-zresvl_princ OR wa_saida_calculo-doc_lcto IS INITIAL.
        MESSAGE 'Não foi gerado documento contábil do cálculo' TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
    "
    IF p_tipo = 'P'.
      IF wa_saida_calculo-belnr_res IS NOT INITIAL.
        MESSAGE 'Gerado documento contábil Residual' TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
    ELSE.
      IF wa_saida_calculo-belnr_resj IS NOT INITIAL.
        MESSAGE 'Gerado documento contábil Residual de Juros' TYPE 'I'.
        wl_erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
    "
  ENDLOOP.

  IF wl_erro = 'X'.
    EXIT.
  ENDIF.

  SORT: it_bsid_aux BY bukrs,
        it_bsik_aux BY bukrs.

  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    wa_saida_calculo-gjahr = p_data(4).
    MODIFY it_saida_calculo FROM wa_saida_calculo INDEX sy-tabix TRANSPORTING gjahr.
  ENDLOOP.

  SELECT *
  FROM bkpf
  INTO TABLE it_bkpf
      FOR ALL ENTRIES IN it_saida_calculo
      WHERE bukrs EQ it_saida_calculo-zid_empresa
      AND   belnr EQ it_saida_calculo-belnr
      AND   gjahr EQ it_saida_calculo-gjahr.
  .
  SELECT *
  FROM bkpf
  APPENDING TABLE it_bkpf
      FOR ALL ENTRIES IN it_saida
      WHERE bukrs EQ it_saida-zid_empresa
      AND   belnr EQ it_saida-zvinc_cia
      AND   gjahr EQ it_saida-gjahr.

  SORT it_bkpf BY bukrs blart.
  DELETE ADJACENT DUPLICATES FROM it_bkpf COMPARING bukrs blart.

*  SELECT *
*  FROM BSEG
*  INTO TABLE IT_BSEG
*      FOR ALL ENTRIES IN IT_SAIDA_CALCULO
*      WHERE BUKRS EQ IT_SAIDA_CALCULO-ZID_EMPRESA
*      AND   BELNR EQ IT_SAIDA_CALCULO-BELNR
*      AND   GJAHR EQ IT_SAIDA_CALCULO-GJAHR
*      AND   BSCHL NOT IN ('40', '50' ).


  "pega documentos originais e calculo de juros
  p_mode = 'N'.
  "
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
        lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
        lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
        lds_return TYPE bapiret2.

  CLEAR: vl_number, wl_zfisolctr.
  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    tabix = sy-tabix.
    REFRESH: lt_blntab,lt_ftclear, lt_ftpost,lt_fttax.
    CLEAR: lt_blntab,lt_ftclear, lt_ftpost,lt_fttax,lds_return.
    "
    SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = wa_saida_calculo-zid_contr.

    lt_ftpost-stype = 'K'."Header
    lt_ftpost-count = 1.  "number of Dynpro

    lt_ftpost-fnam = 'BKPF-BUKRS'.
    lt_ftpost-fval = wa_saida_calculo-zid_empresa.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-WAERS'.
    lt_ftpost-fval = wl_zfitaxctr-waers.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-BLDAT'.
    CONCATENATE p_data+6(2) p_data+4(2) p_data(4) INTO lt_ftpost-fval SEPARATED BY '.'.
    APPEND lt_ftpost.

**********************************************************************
*    Busca taxa do 1º dia do próximo mês
**********************************************************************
    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
          i_data          TYPE gdatu_inv,
          e_ukurs         TYPE ukurs_curr,
          wl_moedas       TYPE x001.

    CREATE OBJECT obj_zcl_util_sd.

    IF ( sy-tcode = 'ZFI0081' ). "Pegar o 1º dia do próximo mês para busca de taxa

      DATA: _olddate TYPE sy-datum, _newdate TYPE sy-datum.

      _olddate = |{ p_data+0(4) }{ p_data+4(2) }01|.

      CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
        EXPORTING
          months  = 1
          olddate = _olddate
        IMPORTING
          newdate = _newdate.

      i_data = _newdate.

      SELECT SINGLE * FROM bkpf INTO @DATA(w_bkpf)
        WHERE bukrs = @wa_saida_calculo-zid_empresa
          AND belnr = @wa_saida_calculo-belnr.

      obj_zcl_util_sd->set_data(  EXPORTING i_data = i_data ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = w_bkpf-waers ).

      SELECT SINGLE bukrs, waers FROM t001 INTO @DATA(w_t001)
          WHERE bukrs = @wa_saida_calculo-zid_empresa.

      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs = w_t001-bukrs
        IMPORTING
          e_x001  = wl_moedas.

      IF ( w_bkpf-waers = w_t001-waers ).
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = wl_moedas-hwae2 ).
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = w_t001-waers ).
      ENDIF.

      obj_zcl_util_sd->taxa_cambio(  RECEIVING e_ukurs = e_ukurs ).

      IF ( e_ukurs IS NOT INITIAL ).
        lt_ftpost-fnam = 'BKPF-KURSF'.
        lt_ftpost-fval = e_ukurs.
        CONDENSE lt_ftpost-fval NO-GAPS.
        REPLACE '.' IN lt_ftpost-fval WITH ','.
        REPLACE '-' IN lt_ftpost-fval WITH ' '.
        CONDENSE lt_ftpost-fval NO-GAPS.
        APPEND lt_ftpost.
      ENDIF.

    ENDIF.

    lt_ftpost-fnam = 'BKPF-BUDAT'.
    CONCATENATE p_data+6(2) p_data+4(2) p_data(4) INTO lt_ftpost-fval SEPARATED BY '.'.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-MONAT'.
    lt_ftpost-fval =  p_data+4(2).
    APPEND lt_ftpost.

    CLEAR vl_qtde.
    LOOP AT it_bkpf WHERE bukrs = wa_saida_calculo-zid_empresa.
      ADD 1 TO vl_qtde.
      v_blart = it_bkpf-blart.
    ENDLOOP.

    IF vl_qtde = 1.
      lt_ftpost-fnam = 'BKPF-BLART'.
      lt_ftpost-fval = v_blart.
      APPEND lt_ftpost.
    ELSE.
      lt_ftpost-fnam = 'BKPF-BLART'.
      lt_ftpost-fval = 'AB'.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BKPF-XBLNR'.
    CONCATENATE 'CTR.' wa_saida_calculo-zid_contr INTO lt_ftpost-fval.
    APPEND lt_ftpost.

    IF wl_zfitaxctr-bukrs_c = wa_saida_calculo-zid_empresa.
      lt_ftclear-agkoa  = 'D'.
      lt_ftclear-agkon = wl_zfitaxctr-kunnr.
      SELECT SINGLE name1 INTO v_name1 FROM kna1 WHERE kunnr = wl_zfitaxctr-kunnr.
      v_gsber   = wl_zfitaxctr-gsber_c.
      v_umskz   = wl_zfitaxctr-umskz_c.
      lt_ftclear-agums = wl_zfitaxctr-umskz_c.
      IF wa_saida_calculo-zresid_result GT 0.
        SELECT SINGLE *
          FROM  tbsl
          INTO wl_tbsl
             WHERE koart EQ 'D'
             AND   xsonu NE space
             AND   shkzg EQ 'S'.
      ELSE.
        SELECT SINGLE *
                 FROM  tbsl
                 INTO wl_tbsl
                    WHERE koart EQ 'D'
                    AND   xsonu NE space
                    AND   shkzg EQ 'H'.
      ENDIF.
    ELSE.
      lt_ftclear-agkoa  = 'K'.
      SELECT SINGLE name1 INTO v_name1 FROM lfa1 WHERE lifnr = wl_zfitaxctr-lifnr.
      lt_ftclear-agkon = wl_zfitaxctr-lifnr.
      v_gsber   = wl_zfitaxctr-gsber_f.
      v_umskz   = wl_zfitaxctr-umskz_f.
      lt_ftclear-agums = wl_zfitaxctr-umskz_f.
      IF wa_saida_calculo-zresid_result GT 0.
        SELECT SINGLE *
          FROM  tbsl
          INTO wl_tbsl
             WHERE koart EQ 'K'
             AND   xsonu NE space
             AND   shkzg EQ 'S'.
      ELSE.
        SELECT SINGLE *
                 FROM  tbsl
                 INTO wl_tbsl
                    WHERE koart EQ 'K'
                    AND   xsonu NE space
                    AND   shkzg EQ 'H'.
      ENDIF.
    ENDIF.
    "
    IF wl_zfitaxctr-waers = 'BRL'. " Juros simples
      "Documento de calculo IR/IOF/JUROS (1 por empresa no mês)
      IF wa_saida_calculo-belnr IS NOT INITIAL.
        lt_ftclear-agbuk  =  wa_saida_calculo-zid_empresa.
        lt_ftclear-xnops  = 'X'.
        lt_ftclear-selfd  = 'BELNR'.
        lt_ftclear-selvon = wa_saida_calculo-belnr.
        APPEND lt_ftclear.
      ENDIF.
    ENDIF.
    "Documentos de solicitações Fornecedor /cliente
    LOOP AT it_saida INTO wa_saida WHERE zid_empresa =  wa_saida_calculo-zid_empresa.
      IF wa_saida-moda NE ''.
        IF wa_saida-moda NE p_tipo.
          CONTINUE.
        ENDIF.
      ENDIF.
      IF wl_zfitaxctr-bukrs_c = wa_saida_calculo-zid_empresa.
        lt_ftclear-agkoa  = 'D'.
      ELSE.
        lt_ftclear-agkoa  = 'K'.
      ENDIF.
      lt_ftclear-agbuk  =  wa_saida_calculo-zid_empresa.
      lt_ftclear-xnops  = 'X'.
      lt_ftclear-selfd  = 'BELNR'.
      lt_ftclear-selvon = wa_saida-zvinc_cia.
      IF wa_saida-buzei IS NOT INITIAL.
        CONCATENATE lt_ftclear-selvon wa_saida-gjahr wa_saida-buzei INTO lt_ftclear-selvon.
      ENDIF.
      APPEND lt_ftclear.
    ENDLOOP.
    " Valor residual
    lt_ftpost-stype = 'P'.
    lt_ftpost-count =  2 .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  wl_tbsl-bschl.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    IF wl_zfitaxctr-bukrs_c = wa_saida_calculo-zid_empresa.
      lt_ftpost-fval = wl_zfitaxctr-kunnr.
    ELSE.
      lt_ftpost-fval = wl_zfitaxctr-lifnr.
    ENDIF.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'RF05A-NEWUM'.
    lt_ftpost-fval =  v_umskz.
    APPEND lt_ftpost.
    IF p_tipo = 'P'.
      IF wa_saida_calculo-zresid_result LT 0.
        wl_vlrn = wa_saida_calculo-zresid_result * -1.
      ELSE.
*---> 08/06/2023 - Migração S4 - JS
*            wl_vlrn = wa_saida_calculo-zresid_result.
        wl_vlrn = CONV #( wa_saida_calculo-zresid_result ).
*<--- 08/06/2023 - Migração S4 - JS

      ENDIF.
    ELSE.
      IF wa_saida_calculo-zresid_resultj LT 0.
        wl_vlrn = wa_saida_calculo-zresid_resultj * -1.
      ELSE.
*---> 08/06/2023 - Migração S4 - JS
*            wl_vlrn = wa_saida_calculo-zresid_resultj.
        wl_vlrn = CONV #( wa_saida_calculo-zresid_resultj ).
*<--- 08/06/2023 - Migração S4 - JS

      ENDIF.
    ENDIF.
    WRITE: wl_vlrn TO wl_vlrc.
    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    IF wl_zfitaxctr-waers = 'BRL'.
      wl_vlrn = wl_vlrn / abs( e_ukurs ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.

    "Busca lançamentos contabeis gerados para pegar moeda interna para caso documento em USD.
    IF wl_zfitaxctr-waers = 'USD'.

      CLEAR vl_moeda_int.

*      LOOP AT IT_BSEG INTO WA_BSEG WHERE BUKRS = WA_SAIDA_CALCULO-ZID_EMPRESA.
*        IF WA_BSEG-SHKZG = 'H'.
*          SUBTRACT WA_BSEG-DMBTR FROM VL_MOEDA_INT.
*        ELSE.
*          ADD WA_BSEG-DMBTR TO VL_MOEDA_INT.
*        ENDIF.
*      ENDLOOP.

* Alteração 19/09/2018
*      LOOP AT IT_BSID_AUX INTO WA_BSID_AUX WHERE BUKRS = WA_SAIDA_CALCULO-ZID_EMPRESA.
*        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ZVINC_CIA =  WA_BSID_AUX-BELNR.
*        IF SY-SUBRC = 0.
*          IF WA_SAIDA-MODA NE P_TIPO.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*        IF WA_BSID_AUX-SHKZG = 'H'.
*          SUBTRACT WA_BSID_AUX-DMBTR FROM VL_MOEDA_INT.
*        ELSE.
*          ADD WA_BSID_AUX-DMBTR TO VL_MOEDA_INT.
*        ENDIF.
*
*      ENDLOOP.
*
*      LOOP AT IT_BSIK_AUX INTO WA_BSIK_AUX WHERE BUKRS = WA_SAIDA_CALCULO-ZID_EMPRESA.
*        READ TABLE IT_SAIDA INTO WA_SAIDA WITH KEY ZVINC_CIA =  WA_BSIK_AUX-BELNR.
*        IF SY-SUBRC = 0.
*          IF WA_SAIDA-MODA NE P_TIPO.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*        IF WA_BSIK_AUX-SHKZG = 'H'.
*          SUBTRACT WA_BSIK_AUX-DMBTR FROM VL_MOEDA_INT.
*        ELSE.
*          ADD WA_BSIK_AUX-DMBTR TO VL_MOEDA_INT.
*        ENDIF.
*      ENDLOOP.
*      IF VL_MOEDA_INT GT 0.
*        WL_VLRN = VL_MOEDA_INT.
*        WRITE: WL_VLRN TO WL_VLRC.
*        LT_FTPOST-FNAM = 'BSEG-DMBTR'.
*        LT_FTPOST-FVAL =  WL_VLRC.
*        APPEND LT_FTPOST.
*      ENDIF.

    ENDIF.

    lt_ftpost-fnam = 'BSEG-GSBER'.
    lt_ftpost-fval =  v_gsber.
    APPEND lt_ftpost.
    "
    lt_ftpost-fnam = 'BSEG-SGTXT'.
    CONCATENATE 'Saldo Residual' v_name1 INTO  lt_ftpost-fval SEPARATED BY space.
    APPEND lt_ftpost.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = l_auglv
        i_tcode                    = l_tcode
        i_sgfunct                  = l_sgfunct
        i_no_auth                  = 'X'
      IMPORTING
        e_msgid                    = lds_return-id
        e_msgno                    = lds_return-number
        e_msgty                    = lds_return-type
        e_msgv1                    = lds_return-message_v1
        e_msgv2                    = lds_return-message_v2
        e_msgv3                    = lds_return-message_v3
        e_msgv4                    = lds_return-message_v4
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.

    IF lt_blntab[] IS INITIAL.
      WRITE lds_return-number TO msg_no.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = lds_return-id
          msg_no                 = msg_no
          msg_var1               = lds_return-message_v1
          msg_var2               = lds_return-message_v2
          msg_var3               = lds_return-message_v3
          msg_var4               = lds_return-message_v4
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      MESSAGE msg_text TYPE 'I'.
      EXIT.
*      RETURN.
    ELSE.
      READ TABLE lt_blntab INDEX 1.
      IF p_tipo = 'P'.
        wa_saida_calculo-belnr_res = lt_blntab-belnr.
        MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING belnr_res.
        "Atualiza doc.contabil
        UPDATE zfirescmu SET belnr_res = lt_blntab-belnr
        WHERE zid_contr     = wa_saida_calculo-zid_contr
        AND   zresdt_calc   = p_data
        AND   zid_empresa   = wa_saida_calculo-zid_empresa.
      ELSE.
        wa_saida_calculo-belnr_resj = lt_blntab-belnr.
        MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING belnr_resj.
        "Atualiza doc.contabil
        UPDATE zfirescmu SET belnr_resj = lt_blntab-belnr
        WHERE zid_contr     = wa_saida_calculo-zid_contr
        AND   zresdt_calc   = p_data
        AND   zid_empresa   = wa_saida_calculo-zid_empresa.
      ENDIF.

      "Gravar na solicitacao
      IF vl_number IS INITIAL.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZDOC_MUTU'
          IMPORTING
            number                  = vl_number
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          wl_zfisolctr-nro_sol = vl_number.
        ENDIF.
      ELSE.
        wl_zfisolctr-nro_sol = vl_number.
      ENDIF.
      v_data = p_data + 1. "mes seguinte

      wl_zfisolctr-zid_contr          = wl_zfitaxctr-zid_contr.
      wl_zfisolctr-bukrs_f            = wl_zfitaxctr-bukrs_f.
      IF p_tipo = 'P'.
        wl_zfisolctr-cd_mod             = 'R'. "residual não tem IOF cabeça
      ELSE.
        wl_zfisolctr-cd_mod             = 'J'. "
      ENDIF.

      wl_zfisolctr-dt_lcto            = v_data.
      wl_zfisolctr-dt_vct             = v_data.
      wl_zfisolctr-waers              = wl_zfitaxctr-waers.
      wl_zfisolctr-vlr_moeda_doc      = 0.
      wl_zfisolctr-hkont_f            = wl_zfitaxctr-hkont_f.
      wl_zfisolctr-hkont_c            = wl_zfitaxctr-hkont_c.
      IF wl_zfitaxctr-bukrs_f = wa_saida_calculo-zid_empresa.
        wl_zfisolctr-belnrf             = lt_blntab-belnr.
        wl_zfisolctr-gjahrf             = lt_blntab-gjahr.
        IF wl_zfitaxctr-bukrs_c = '9999'.
          wl_zfisolctr-bukrs_c = '9999'.
        ENDIF.
      ELSE.
        wl_zfisolctr-bukrs_c            = lt_blntab-bukrs.
        wl_zfisolctr-belnrc             = lt_blntab-belnr.
        wl_zfisolctr-gjahrc             = lt_blntab-gjahr.
        IF wl_zfitaxctr-bukrs_f = '9999'.
          wl_zfisolctr-bukrs_f = '9999'.
        ENDIF.
      ENDIF.
      MOVE : sy-mandt                    TO wl_zfisolctr-mandt,
             sy-uname                    TO wl_zfisolctr-usnam,
             sy-datum                    TO wl_zfisolctr-dt_entrada,
             sy-uzeit                    TO wl_zfisolctr-hr_entrada.
    ENDIF.
    "
  ENDLOOP.
  IF wl_zfisolctr-belnrf IS NOT INITIAL OR wl_zfisolctr-belnrc IS NOT INITIAL.
    MODIFY zfisolctr FROM       wl_zfisolctr.
  ENDIF.
  "
  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    EXIT.
*      RETURN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MEMORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_memoria.
  CHECK it_zfictrmu[] IS NOT INITIAL.

  REFRESH it_zficalcmu.
  SELECT *
    FROM zficalcmu
    INTO TABLE it_zficalcmu
    FOR ALL ENTRIES IN it_zfictrmu
    WHERE zid_contr   = it_zfictrmu-zid_contr
    AND   zid_empresa = it_zfictrmu-zid_empresa
    AND   zvinc_cia   = it_zfictrmu-zvinc_cia.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno .
  DATA: vdata(10),
        p_erro(1),
        vobj_key     TYPE zib_contabil-obj_key,
        tabix        TYPE sy-tabix,
        v_data       TYPE sy-datum,
        wl_zfitaxctr TYPE zfitaxctr,
        wl_zfisolctr TYPE zfisolctr.

  CLEAR p_erro.
  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    IF wa_saida_calculo-belnr_res IS NOT INITIAL.
      MESSAGE 'Estorne primeiro a compensação' TYPE 'I'.
      p_erro = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF p_erro = 'X'.
    EXIT.
  ENDIF.
  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    tabix = sy-tabix.
    IF wa_saida_calculo-belnr IS NOT INITIAL.
      CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vdata.
      REFRESH ti_bdcdata.
      PERFORM f_bdc_data USING:
            'SAPMF05A'  '0105'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'	      '/00',
            ''          ''      ''   'RF05A-BELNS'      wa_saida_calculo-belnr,
            ''          ''      ''   'BKPF-BUKRS'       wa_saida_calculo-zid_empresa,
            ''          ''      ''   'RF05A-GJAHS'      p_data(4),
            ''          ''      ''   'UF05A-STGRD'      '01',
*            ''          ''      ''   'BSIS-BUDAT'       VDATA,
            'SAPMF05A'  '0105'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'	      '=BU'.

      CLEAR p_erro.

      PERFORM zf_call_transaction USING 'FB08' CHANGING p_erro.
      IF wg_documento IS NOT INITIAL.
        CLEAR: wa_saida_calculo-belnr, wa_saida_calculo-doc_lcto.
        MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING belnr doc_lcto.
        "Apaga tabelas
        "Juros
        SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = p_ctr.
        IF wl_zfitaxctr-waers NE 'BRL'. "ALRS
          v_data = p_data + 1. "mes seguinte
          CONCATENATE 'ZGL17' wa_saida_calculo-doc_lcto p_data+0(4) INTO vobj_key.
          SELECT SINGLE *
           FROM zib_contabil_chv
           INTO  wa_zib_contabil_chv
           WHERE obj_key EQ vobj_key.
          IF wl_zfitaxctr-bukrs_f =  wa_saida_calculo-zid_empresa.
            DELETE FROM zfisolctr
             WHERE bukrs_f  = wa_zib_contabil_chv-bukrs
             AND   belnrf   = wa_zib_contabil_chv-belnr
             AND   gjahrf   = wa_zib_contabil_chv-gjahr
             AND   dt_lcto  = v_data.
          ELSE.
            DELETE FROM zfisolctr
            WHERE bukrs_c  = wa_zib_contabil_chv-bukrs
            AND   belnrc   = wa_zib_contabil_chv-belnr
            AND   gjahrc   = wa_zib_contabil_chv-gjahr
            AND   dt_lcto  = v_data.
          ENDIF.
        ENDIF.
        "
        DELETE FROM zfirescmu
         WHERE zid_contr     = wa_saida_calculo-zid_contr
         AND   zresdt_calc   = p_data
         AND   zid_empresa   = wa_saida_calculo-zid_empresa.

        DELETE FROM zfictrmu
          WHERE zid_contr     = wa_saida_calculo-zid_contr
          AND   zid_empresa   = wa_saida_calculo-zid_empresa
          AND   zresdt_calc   = wa_zfirescmu-zresdt_calc.
        COMMIT WORK.
      ENDIF.
    ELSEIF wa_saida_calculo-doc_lcto IS NOT INITIAL.
      CLEAR: wa_saida_calculo-belnr, wa_saida_calculo-doc_lcto.
      MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING belnr doc_lcto.
      "Apaga tabelas
      DELETE FROM zfirescmu
       WHERE zid_contr     = wa_saida_calculo-zid_contr
       AND   zresdt_calc   = p_data
       AND   zid_empresa   = wa_saida_calculo-zid_empresa.

      DELETE FROM zfictrmu
        WHERE zid_contr     = wa_saida_calculo-zid_contr
        AND   zid_empresa   = wa_saida_calculo-zid_empresa
        AND   zresdt_calc   = wa_zfirescmu-zresdt_calc.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5106   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont    TYPE sy-tabix,
        wl_mode(1).

  REFRESH it_msg .

  wl_mode = 'E'.
  IF p_trans = 'F-53'.
    CALL TRANSACTION p_trans USING ti_bdcdata
      MODE wl_mode
      MESSAGES INTO it_msg
      UPDATE 'S'.
  ELSE.
    CALL TRANSACTION p_trans USING ti_bdcdata
          MODE wl_mode
          MESSAGES INTO it_msg.
  ENDIF.
  CLEAR: wl_cont.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.
  IF p_trans = 'FBRA'.
    READ TABLE it_msg WITH KEY msgid = c_msgid
                           msgnr = c_msgne
                           msgtyp = 'S'.
  ELSE.
    READ TABLE it_msg WITH KEY msgid = c_msgid
                               msgnr = c_msgnr
                               msgtyp = 'S'.
  ENDIF.
  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION

FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_c .
  DATA: vdata(10),
        p_erro(1),
        tabix      TYPE sy-tabix,
        p_mode     LIKE rfpdo-allgazmd,
        vmsg_id    LIKE sy-msgid,
        vmsgty     LIKE sy-msgty,
        vmsg_no    LIKE sy-msgno,
        vmsg_var1  LIKE sy-msgv1,
        vmsg_var2  LIKE sy-msgv2,
        vmsg_var3  LIKE sy-msgv3,
        vmsg_var4  LIKE sy-msgv4,
        vsubrc     LIKE sy-subrc,
        wa_bapiret LIKE bapiret2,

        pmsg_id  	 LIKE	t100-arbgb,
        pmsg_no	   LIKE	t100-msgnr,
        pmsg_var1	 LIKE	balm-msgv1,
        pmsg_var2	 LIKE	balm-msgv2,
        pmsg_var3	 LIKE	balm-msgv3,
        pmsg_var4	 LIKE	balm-msgv4,

        i_augbl    LIKE  rf05r-augbl,
        i_bukrs    LIKE  rf05r-bukrs,
        i_gjahr    LIKE  rf05r-gjahr,
        lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE.

  p_mode = 'N'.
  "
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  LOOP AT it_saida_calculo INTO wa_saida_calculo.
    REFRESH lt_blntab.
    CLEAR lt_blntab.
    tabix = sy-tabix.
    i_augbl = wa_saida_calculo-belnr_res.
    i_bukrs = wa_saida_calculo-zid_empresa.
    i_gjahr = p_data(4).
    CALL FUNCTION 'POSTING_INTERFACE_RESET_CLEAR'
      EXPORTING
        i_augbl                  = i_augbl
        i_bukrs                  = i_bukrs
        i_gjahr                  = i_gjahr
        i_tcode                  = 'FBRA'
*       I_NO_AUTH                = ' '
      IMPORTING
        e_msgid                  = vmsg_id
        e_msgno                  = vmsg_no
        e_msgty                  = vmsgty
        e_msgv1                  = vmsg_var1
        e_msgv2                  = vmsg_var2
        e_msgv3                  = vmsg_var3
        e_msgv4                  = vmsg_var4
        e_subrc                  = vsubrc
      EXCEPTIONS
        transaction_code_invalid = 1
        no_authorization         = 2
        OTHERS                   = 3.

    CLEAR wa_bapiret-message.
    pmsg_id     = vmsg_id.
    pmsg_no     = vmsg_no.
    pmsg_var1   = vmsg_var1.
    pmsg_var2   = vmsg_var2.
    pmsg_var3   = vmsg_var3.
    pmsg_var4   = vmsg_var4.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language = sy-langu
        msg_id   = pmsg_id
        msg_no   = pmsg_no
        msg_var1 = pmsg_var1
        msg_var2 = pmsg_var2
        msg_var3 = pmsg_var3
        msg_var4 = pmsg_var4
      IMPORTING
        msg_text = wa_bapiret-message.

    IF vsubrc EQ 0.
      CLEAR wa_saida_calculo-belnr_res.
      MODIFY it_saida_calculo FROM wa_saida_calculo INDEX tabix TRANSPORTING belnr_res.
      "Atualiza doc.contabil
      UPDATE zfirescmu SET belnr_res = ''
      WHERE zid_contr     = wa_saida_calculo-zid_contr
      AND   zresdt_calc   = p_data
      AND   zid_empresa   = wa_saida_calculo-zid_empresa.
      "
*      CALL FUNCTION 'POSTING_INTERFACE_REVERSE_DOC'
*        EXPORTING
*          I_BELNS                  = I_AUGBL
*          I_BUKRS                  = I_BUKRS
*          I_TCODE                  = 'FB08'
*          I_STGRD                  = '01'
*        TABLES
*          T_BLNTAB                 = LT_BLNTAB
*        EXCEPTIONS
*          TRANSACTION_CODE_INVALID = 1
*          NO_AUTHORIZATION         = 2
*          OTHERS                   = 3.
*      IF LT_BLNTAB[] IS INITIAL.

      CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO vdata.
      REFRESH ti_bdcdata.
      PERFORM f_bdc_data USING:
            'SAPMF05A'  '0105'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'	      '/00',
            ''          ''      ''   'RF05A-BELNS'      i_augbl,
            ''          ''      ''   'BKPF-BUKRS'       i_bukrs,
            ''          ''      ''   'RF05A-GJAHS'      i_gjahr,
            ''          ''      ''   'UF05A-STGRD'      '01',
            'SAPMF05A'  '0105'  'X'  ''                 ' ',
            ''          ''      ''   'BDC_OKCODE'	      '=BU'.

      CLEAR: wg_documento, p_erro.

      PERFORM zf_call_transaction USING 'FB08' CHANGING p_erro.
      IF wg_documento IS INITIAL.
        MESSAGE 'Anulação executada, houve erro na reversão FB08' TYPE 'I'.
      ELSE.
*        READ TABLE LT_BLNTAB INDEX 1.
*        CONCATENATE WA_BAPIRET-MESSAGE 'Est. ' LT_BLNTAB-BELNR INTO WA_BAPIRET-MESSAGE SEPARATED BY SPACE.
        CONCATENATE 'Est. ' wg_documento INTO wa_bapiret-message SEPARATED BY space.
        MESSAGE wa_bapiret-message TYPE 'I'.
      ENDIF.
    ELSE.
      MESSAGE wa_bapiret-message TYPE 'I'.
    ENDIF.
  ENDLOOP.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    EXIT.
*      RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRANSPORTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_transportar USING p_tipo .
  DATA: vl_number    TYPE i,
        wl_erro(1),
        wl_erro2(1),
        v_data       TYPE sy-datum,
        wl_zfitaxctr TYPE zfitaxctr,
        wl_zfisolctr TYPE zfisolctr.
  "
  IF it_saida_calculo[] IS INITIAL.
    MESSAGE 'Não foi gerado Contabilização' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR: wl_erro, wl_erro2.
  IF p_tipo = 'T'.
    LOOP AT it_saida_calculo INTO wa_saida_calculo.
      IF wa_saida_calculo-belnr IS INITIAL.
        MESSAGE 'Não foi gerado documento contábil do cálculo' TYPE 'I'.
        wl_erro  = 'X'.
        wl_erro2 = 'X'.
        EXIT.
      ENDIF.
      "
      IF wa_saida_calculo-belnr_res IS NOT INITIAL.
        MESSAGE 'Gerado documento contábil Residual' TYPE 'I'.
        wl_erro = 'X'.
        " EXIT.
      ENDIF.
      "
      IF wa_saida_calculo-belnr_resj IS NOT INITIAL.
        MESSAGE 'Gerado documento contábil Juros Residual' TYPE 'I'.
        wl_erro2 = 'X'.
        "EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wl_erro = 'X' AND wl_erro2 = 'X'.
    EXIT.
  ENDIF.

  v_data = p_data + 1. "mes seguinte
  CLEAR vl_number.
  IF p_tipo = 'E'.
    LOOP AT it_saida INTO wa_saida.
      SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = wa_saida-zid_contr.
      IF wl_zfitaxctr-bukrs_f = wa_saida-zid_empresa.
        DELETE FROM zfisolctr
        WHERE bukrs_f  = wl_zfitaxctr-bukrs_f
        AND   belnrf   = wa_saida-zvinc_cia
        AND   gjahrf   = wa_saida-gjahr
        AND   dt_lcto  = v_data.
        IF sy-subrc = 0.
          wl_erro = 'X'.
        ENDIF.
      ELSE.
        DELETE FROM zfisolctr
        WHERE bukrs_c  = wl_zfitaxctr-bukrs_c
        AND   belnrc   = wa_saida-zvinc_cia
        AND   gjahrc   = wa_saida-gjahr
        AND   dt_lcto  = v_data.
        IF sy-subrc = 0.
          wl_erro = 'X'.
        ENDIF.
      ENDIF.
      COMMIT WORK.
    ENDLOOP.
    IF wl_erro = 'X'.
      MESSAGE 'Transporte estornado' TYPE 'I'.
    ELSE.
      MESSAGE 'Não transporte a estornar!' TYPE 'I'.
    ENDIF.
  ELSE.
    LOOP AT it_saida INTO wa_saida.
      "
      IF wa_saida-moda = 'J' AND wl_erro2 = 'X'.
        CONTINUE.
      ENDIF.
      IF wa_saida-moda NE  'J' AND wl_erro = 'X'.
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM zfitaxctr INTO wl_zfitaxctr WHERE zid_contr = wa_saida-zid_contr.
      IF wl_zfitaxctr-bukrs_f = wa_saida-zid_empresa.
        SELECT SINGLE *
          FROM zfisolctr
          INTO wl_zfisolctr
          WHERE bukrs_f  = wl_zfitaxctr-bukrs_f
          AND   belnrf   = wa_saida-zvinc_cia
          AND   gjahrf   = wa_saida-gjahr
          AND   dt_lcto  = v_data.
      ELSE.
        SELECT SINGLE *
          FROM zfisolctr
          INTO wl_zfisolctr
          WHERE bukrs_c  = wl_zfitaxctr-bukrs_c
          AND   belnrc   = wa_saida-zvinc_cia
          AND   gjahrc   = wa_saida-gjahr
          AND   dt_lcto  = v_data.
      ENDIF.
      IF sy-subrc = 0.
        MESSAGE 'Transporte já realizado' TYPE 'I'.
        EXIT.
      ENDIF.

      "Gravar na solicitacao
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZDOC_MUTU'
        IMPORTING
          number                  = vl_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        wl_zfisolctr-nro_sol = vl_number.
      ENDIF.

      wl_zfisolctr-zid_contr          = wl_zfitaxctr-zid_contr.
      wl_zfisolctr-bukrs_f            = wl_zfitaxctr-bukrs_f.
      IF wa_saida-moda = 'J'.
        wl_zfisolctr-cd_mod             = 'J'. "juros
      ELSE.
        wl_zfisolctr-cd_mod             = 'R'. "residual
      ENDIF.
      wl_zfisolctr-dt_lcto            = v_data.
      wl_zfisolctr-dt_vct             = v_data.
      wl_zfisolctr-waers              = wl_zfitaxctr-waers.
      wl_zfisolctr-vlr_moeda_doc      = 0.
      wl_zfisolctr-hkont_f            = wl_zfitaxctr-hkont_f.
      wl_zfisolctr-hkont_c            = wl_zfitaxctr-hkont_c.
      IF wl_zfitaxctr-bukrs_f = wa_saida-zid_empresa.
        wl_zfisolctr-belnrf             = wa_saida-zvinc_cia.
        wl_zfisolctr-gjahrf             = wa_saida-gjahr.
        IF wl_zfitaxctr-bukrs_c = '9999'.
          wl_zfisolctr-bukrs_c = '9999'.
        ENDIF.
      ELSE.
        wl_zfisolctr-bukrs_c            = wl_zfitaxctr-bukrs_c.
        wl_zfisolctr-belnrc             = wa_saida-zvinc_cia.
        wl_zfisolctr-gjahrc             = wa_saida-gjahr.
        IF wl_zfitaxctr-bukrs_f = '9999'.
          wl_zfisolctr-bukrs_f = '9999'.
        ENDIF.
      ENDIF.
      MOVE : sy-mandt                    TO wl_zfisolctr-mandt,
             sy-uname                    TO wl_zfisolctr-usnam,
             sy-datum                    TO wl_zfisolctr-dt_entrada,
             sy-uzeit                    TO wl_zfisolctr-hr_entrada.
      MODIFY zfisolctr FROM wl_zfisolctr.
      COMMIT WORK.
    ENDLOOP.
    IF vl_number IS NOT INITIAL.
      MESSAGE 'Transporte concluido' TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.
