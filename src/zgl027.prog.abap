************************************************************************
* TÍTULO     : Avaliação de Moeda Estrangeira                          *
* TIPO PROG  : REPORT - ZGL027                                         *
* FUNCIONAL  : Cleandra Piovesan                                       *
* AUTOR(A)   : Marcos Faneli                                           *
* DATA       : 03.03.2015                                              *
* TRANSAÇÃO  : ZGL042                                                  *
* DESCRIÇÃO  : Avaliação de Moeda Estrangeira                          *
************************************************************************
REPORT  zgl027 MESSAGE-ID z01 NO STANDARD PAGE HEADING.
INCLUDE <icon>.

*CONTROLS MY  TABSTRIP TYPE TABSTRIP.
CONSTANTS: "C_USD  TYPE TCURR_CURR VALUE 'USD',
           c_0050 TYPE ktopl      VALUE '0050'.

DATA: c_usd        TYPE tcurr_curr VALUE 'USD',
      ok_code      TYPE sy-ucomm,
      save_ok      TYPE sy-ucomm,
      wg_waers     TYPE c LENGTH 3 VALUE 'USD',
      wg_indicador TYPE c,
      vseqitem     TYPE zib_contabil-seqitem,
      vnum(10)     TYPE c,
      vseq(10)     TYPE p,
      vobj_key     TYPE zgl012_avm-obj_key,
      vg_last_day  TYPE sy-datum.

DATA  number TYPE sy-dynnr.
*TABELAS STANDARD
TABLES: bsik,     "Cont.: índice secund. para fornecedores
        bsid,     "Cont.: índice secund. para clientes
        bsis.     "Cont. financ.: índice secund. p/contas do Razão

TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil.

TYPES: BEGIN OF ty_zib_contabil_err.
         INCLUDE STRUCTURE zib_contabil_err.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil_err.

TYPES: BEGIN OF ty_zib_contabil_chv.
         INCLUDE STRUCTURE zib_contabil_chv.
TYPES:   bldat TYPE zib_contabil-bldat,
       END OF ty_zib_contabil_chv.

*TIPOS DE REFERÊNCIA
TYPES: BEGIN OF ty_bsik,
         bukrs     TYPE bsik-bukrs,
         lifnr     TYPE bsik-lifnr,
         umskz     TYPE bsik-umskz,
         belnr     TYPE bsik-belnr,
         buzei     TYPE bsik-buzei,
         budat     TYPE bsik-budat,
         waers     TYPE bsik-waers,
         blart     TYPE blart,
         bschl     TYPE bschl,
         shkzg     TYPE shkzg,
         gsber     TYPE gsber,
         dmbtr     TYPE bsik-dmbtr,
         wrbtr     TYPE bsik-wrbtr,
         hkont     TYPE bsik-hkont,
         dmbe2     TYPE bsik-dmbe2,
         gjahr     TYPE bsik-gjahr,
         augbl     TYPE bsik-augbl,
         augdt     TYPE bsik-augdt,
         st_rev(1),
       END OF ty_bsik,

       BEGIN OF ty_bsid,
         bukrs     TYPE bsid-bukrs,
         kunnr     TYPE bsid-kunnr,
         umskz     TYPE bsid-umskz,
         belnr     TYPE bsid-belnr,
         buzei     TYPE bsid-buzei,
         budat     TYPE bsid-budat,
         waers     TYPE bsid-waers,
         blart     TYPE blart,
         bschl     TYPE bschl,
         shkzg     TYPE shkzg,
         gsber     TYPE gsber,
         dmbtr     TYPE bsid-dmbtr,
         wrbtr     TYPE bsid-wrbtr,
         hkont     TYPE bsid-hkont,
         dmbe2     TYPE bsid-dmbe2,
         gjahr     TYPE bsid-gjahr,
         augbl     TYPE bsid-augbl,
         augdt     TYPE bsid-augdt,
         st_rev(1),
       END OF ty_bsid,

       BEGIN OF ty_bsis,
         bukrs     TYPE bsis-bukrs,
         hkont     TYPE bsis-hkont,
         belnr     TYPE bsis-belnr,
         buzei     TYPE bsis-buzei,
         budat     TYPE bsis-budat,
         waers     TYPE bsis-waers,
         blart     TYPE blart,
         bschl     TYPE bschl,
         shkzg     TYPE shkzg,
         gsber     TYPE gsber,
         dmbtr     TYPE bsis-dmbtr,
         wrbtr     TYPE bsis-wrbtr,
         dmbe2     TYPE bsis-dmbe2,
         gjahr     TYPE bsis-gjahr,
         augbl     TYPE bsis-augbl,
         augdt     TYPE bsis-augdt,
         st_rev(1),
       END OF ty_bsis,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         budat TYPE bkpf-budat,
         stblg TYPE bkpf-stblg,
         stjah TYPE bkpf-stjah,
       END OF ty_bkpf,

       BEGIN OF ty_t030h,
         ktopl TYPE t030h-ktopl,
         hkont TYPE t030h-hkont,
         waees TYPE t030h-waers,
         curtp TYPE t030h-curtp,
         lsbew TYPE t030h-lsbew,
         lhbew TYPE t030h-lhbew,
       END OF ty_t030h,

       BEGIN OF ty_tcurr,
         kurst TYPE tcurr-kurst,
         fcurr TYPE tcurr-fcurr,
         tcurr TYPE tcurr-tcurr,
         gdatu TYPE tcurr-gdatu,
         ukurs TYPE tcurr-ukurs,
       END OF ty_tcurr,

       BEGIN OF ty_t001b,
         frpe1 TYPE t001b-frpe1,
         frye1 TYPE t001b-frye1,
       END OF   ty_t001b,


       BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         land1 TYPE t001-land1,
       END OF   ty_t001,

       BEGIN OF ty_t005,
         land1 TYPE t005-land1,
         waers TYPE t005-waers,
         curin TYPE t005-curin,
         curha TYPE t005-curha,
       END OF   ty_t005,

       BEGIN OF ty_skb1,
         bukrs TYPE skb1-bukrs,
         saknr TYPE skb1-saknr,
         xopvw TYPE skb1-xopvw,
       END OF  ty_skb1.


DATA: BEGIN OF t_zgl012_avm OCCURS 0.
        INCLUDE STRUCTURE zgl012_avm.
DATA: dele(1).
DATA: END OF   t_zgl012_avm.

DATA: BEGIN OF t_zgl012_atual OCCURS 0.
        INCLUDE STRUCTURE zgl012_avm.
DATA: dele(1).
DATA: END OF   t_zgl012_atual.

DATA: BEGIN OF t_zgl012_aux OCCURS 0.
        INCLUDE STRUCTURE zgl012_avm.
DATA: dele(1).
DATA: END OF   t_zgl012_aux.
*

*DECLARAÇÃO DE TABELA INTERNA
DATA: t_bsik              TYPE TABLE OF ty_bsik,
      it_bkpf             TYPE TABLE OF bkpf WITH HEADER LINE,
      it_bkpf_est         TYPE TABLE OF bkpf WITH HEADER LINE,

      t_bsid              TYPE TABLE OF ty_bsid,
      t_bsis              TYPE TABLE OF ty_bsis,
      t_t030h             TYPE TABLE OF ty_t030h,
      t_tcurr             TYPE TABLE OF ty_tcurr,
      t_tcurr_a           TYPE TABLE OF ty_tcurr,
      t_tcurr_g           TYPE TABLE OF ty_tcurr,
      t_t001b             TYPE TABLE OF ty_t001b,
      t_t001              TYPE TABLE OF ty_t001,
      t_t005              TYPE TABLE OF ty_t005,
      t_skb1              TYPE TABLE OF ty_skb1,
      t_0025              TYPE TABLE OF zfit0025,
      tg_t882g            TYPE TABLE OF t882g,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil,
      it_zib_contabil_err TYPE TABLE OF ty_zib_contabil_err,
      it_zib_contabil_chv TYPE TABLE OF ty_zib_contabil_chv.

*DECLARAÇÃO DE WORK AREA
DATA: wa_bsik             TYPE ty_bsik,
      wa_zgl012_avm       LIKE t_zgl012_avm,
      wa_zgl012_2         LIKE t_zgl012_avm,
      wa_zgl012_aux       LIKE t_zgl012_avm,
      wa_bsid             TYPE ty_bsid,
      wa_bsis             TYPE ty_bsis,
      wa_t030h            TYPE ty_t030h,
      wa_tcurr            TYPE ty_tcurr,
      wa_t001b            TYPE ty_t001b,
      wa_t001             TYPE ty_t001,
      wa_t005             TYPE ty_t005,
      wa_skb1             TYPE ty_skb1,
      wa_0025             TYPE zfit0025,
      wg_t882g            TYPE t882g,
      wa_zib_contabil     TYPE ty_zib_contabil,
      wa_zib_contabil_err TYPE ty_zib_contabil_err,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
      wg_bkpf_fb08        TYPE ty_bkpf,
      wg_bkpf_fb08_e      TYPE ty_bkpf.


*DECLARAÇÃO DE VARIÁVEIS DE TAXA DE CÂMBIO
DATA: xtx_usd      TYPE tcurr-ukurs,
      xtx_usd_aux  TYPE tcurr-ukurs,
      xtx_usd_a    TYPE tcurr-ukurs,
      wg_dmbt2     TYPE bsik-dmbt2 VALUE '0.01',
      wg_cur_p     TYPE waers, " Variável que armazena a moeda principal
      wl_st_rev(1),
      wg_acao(30).

DATA: it_bdcdata TYPE bdcdata OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF o_alv2 OCCURS 0, "TABELA DE SAIDA DO ALV
        mark,
        racct       TYPE faglflext-racct,
        rassc       TYPE faglflext-rassc,
        txt50       TYPE skat-txt50,
        waers       TYPE zgl012_avm-waers,
        ktoks       TYPE ska1-ktoks,
        moeda_atu   TYPE z_char12,
        wrbtr       TYPE zgl012_avm-wrbtr,
        curr1       TYPE faglflext-hslvt,
        curr2       TYPE faglflext-kslvt,
        curr3       TYPE faglflext-oslvt,
        tx_usd      TYPE zfit0082-tx_usd,
        tx_brl      TYPE zfit0082-tx_brl,
        saldo_corr  TYPE faglflext-hslvt,
        saldo_corr2 TYPE faglflext-hslvt,
        vlr_ajust   TYPE faglflext-kslvt,
        vlr_ajust2  TYPE faglflext-kslvt,
        belnr       TYPE zib_contabil_chv-belnr,
        belnr_est   TYPE zib_contabil_chv-belnr,
        obj_key     TYPE zib_contabil_chv-obj_key,
        obj_key_est TYPE zib_contabil_chv-obj_key,
        log(4),
      END OF o_alv2.


DATA : e_status(1),
       e_messa(64).

DATA: BEGIN OF o_alv OCCURS 0, "TABELA DE SAIDA DO ALV
        mark(1),
        mandt            TYPE zgl012_avm-mandt,
        bukrs            TYPE zgl012_avm-bukrs,
        dt_aval          TYPE zgl012_avm-dt_aval,
        kunnr            TYPE zgl012_avm-kunnr,
        belnr            TYPE zgl012_avm-belnr,
        buzei            TYPE zgl012_avm-buzei,
        moeda_atu        TYPE z_char12,
        budat            TYPE zgl012_avm-budat,
        waers            TYPE zgl012_avm-waers,
        gsber            TYPE zgl012_avm-gsber,
        dmbtr            TYPE zgl012_avm-dmbtr,
        dmbe2            TYPE zgl012_avm-dmbe2,
        wrbtr            TYPE zgl012_avm-wrbtr,
        kursf            TYPE zgl012_avm-kursf,
        vlr_atualizado   TYPE zgl012_avm-vlr_atualizado,
        vlr_acum_mes_atu TYPE zgl012_avm-vlr_acum_mes_atu,
        vlr_acum_mes_ant TYPE zgl012_avm-vlr_acum_mes_ant,
        vlr_variacao     TYPE zgl012_avm-vlr_variacao,
        resultado        TYPE zgl012_avm-resultado,
        tx_fech          TYPE zgl012_avm-tx_fech,
        hkont            TYPE zgl012_avm-hkont,
        umskz            TYPE zgl012_avm-umskz,
        lifnr            TYPE zgl012_avm-lifnr,
        dt_lcto          TYPE zgl012_avm-dt_lcto,
        doc_lcto         TYPE zgl012_avm-doc_lcto,
        cpudt            TYPE zgl012_avm-cpudt,
        cputm            TYPE zgl012_avm-cputm,
        usnam            TYPE zgl012_avm-usnam,
        augdt            TYPE zgl012_avm-augdt,
        bschl            TYPE zgl012_avm-bschl,
        augbl            TYPE zgl012_avm-augbl,
        obj_key          TYPE zgl012_avm-obj_key,
        st_rev           TYPE zgl012_avm-st_rev,
        doc_rev          TYPE zgl012_avm-doc_rev,
        dt_rev           TYPE zgl012_avm-dt_rev,
        status(4),
        tipo(10),
        codigo(10),
        descricao(50),
        resultado_c(7),
        estorno(1),
        dele(1),
      END OF o_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE zgl012_avm-bukrs OBLIGATORY,
              p_budat TYPE zgl012_avm-budat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"100
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_c_lanc RADIOBUTTON GROUP r1 DEFAULT 'X'."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 4(20) TEXT-c02 FOR FIELD p_c_lanc. "P_EX_TES.
    SELECTION-SCREEN POSITION 24.
    PARAMETERS: p_e_lanc RADIOBUTTON GROUP r1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 26(20) TEXT-c03 FOR FIELD p_e_lanc. "P_C_LANC.
    SELECTION-SCREEN POSITION 50.
    PARAMETERS: p_v_lanc RADIOBUTTON GROUP r1."AS CHECKBOX.
    SELECTION-SCREEN COMMENT 65(20) TEXT-c06 FOR FIELD p_v_lanc. "P_V_LANC.
    SELECTION-SCREEN POSITION 54.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: p_budat2 TYPE zgl012_avm-budat,
              p_augdt  TYPE zgl012_avm-augdt,
              p_spmon  TYPE spmon,
              p_blart  TYPE bsas-blart DEFAULT TEXT-004,
              p_sgtxt  TYPE bsik-sgtxt.

SELECTION-SCREEN END OF BLOCK b2.
"200
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_hkont RADIOBUTTON GROUP rad1 DEFAULT 'X' .
  SELECT-OPTIONS: s_hkont FOR bsik-hkont.
  PARAMETERS: p_lifnr RADIOBUTTON GROUP rad1.
  SELECT-OPTIONS: s_lifnr FOR bsik-lifnr.
  SELECT-OPTIONS: s_kont  FOR bsik-hkont.
  PARAMETERS: p_kunnr RADIOBUTTON GROUP rad1.
  SELECT-OPTIONS: s_kunnr FOR bsid-kunnr.
  SELECT-OPTIONS: s_dont  FOR bsid-hkont.
  SELECT-OPTIONS: s_belnr FOR bsik-belnr MODIF ID sc1.
  PARAMETERS: p_hkont2 RADIOBUTTON GROUP rad1.
  SELECT-OPTIONS: s_hkont2 FOR bsik-hkont.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b4.


**********Declarações ALV*****************************
*Pool de declarações do ALV.
TYPE-POOLS: kkblo,
            slis,
            shlp,
            vrm.

*Estrutura
DATA: st_selfield TYPE kkblo_selfield.

************************************************************************
* C O N T R O L E   A L V
************************************************************************
DATA: editcontainer    TYPE REF TO cl_gui_custom_container,
      cl_container     TYPE REF TO cl_gui_custom_container,
      editor           TYPE REF TO cl_gui_textedit,
      cl_container_95  TYPE REF TO cl_gui_docking_container,
      cl_container_05  TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id    TYPE REF TO cl_dd_document,
      cl_grid          TYPE REF TO cl_gui_alv_grid,
      wa_afield        TYPE lvc_s_fcat,
      it_fieldcat      TYPE lvc_t_fcat,
      i_sort           TYPE lvc_t_sort,
      wa_layout        TYPE lvc_s_layo,
      is_stable        TYPE lvc_s_stbl VALUE 'XX',
      wg_repname       LIKE sy-repid,
      wg_x_variant     LIKE disvariant,
      wg_exit(1)       TYPE c,
      wg_save(1)       TYPE c,
      wg_documento(10),
      wg_variant       LIKE disvariant.

DATA: BEGIN OF src OCCURS 5,
        line(100),
      END OF src.

*Tabelas internas do ALV.
DATA: it_fcat   TYPE slis_t_fieldcat_alv,
      it_header TYPE kkblo_t_listheader,
      it_sort   TYPE slis_t_sortinfo_alv WITH HEADER LINE.


************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_row_id
          e_column_id
          es_row_no.

    METHODS:
      handle_top_of_page
        FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index.

ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD catch_hotspot.
    DATA: wl_ano(4).

    TYPES: BEGIN OF ty_itab ,
             name(80) TYPE c,
           END OF ty_itab.

    DATA: msg_alv  TYPE char80,
          itab_msg TYPE TABLE OF ty_itab,
          wtab_msg TYPE  ty_itab.

    IF p_hkont2 = 'X'.
      READ TABLE o_alv2 INTO o_alv2 INDEX e_row_id-index.
      IF sy-subrc = 0.
        IF e_column_id = 'BELNR' AND o_alv2-belnr IS NOT INITIAL.
          wl_ano = p_budat(4).
          SET PARAMETER ID 'BLN' FIELD o_alv2-belnr.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          SET PARAMETER ID 'GJR' FIELD p_budat(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF e_column_id = 'BELNR_EST' AND o_alv2-belnr_est IS NOT INITIAL.
          wl_ano = p_budat(4).
          SET PARAMETER ID 'BLN' FIELD o_alv2-belnr_est.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          SET PARAMETER ID 'GJR' FIELD p_budat(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF o_alv2-log = icon_incomplete.

          SELECT *
             FROM zib_contabil_err
             INTO TABLE it_zib_contabil_err
             WHERE obj_key  = o_alv2-obj_key.

          wtab_msg-name    = '------------------------MENSAGEM ERRO---------------------------------------'.
          APPEND wtab_msg TO itab_msg .
          CLEAR wtab_msg.
          LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
            wtab_msg-name = wa_zib_contabil_err-message.
            APPEND wtab_msg TO itab_msg .
            CLEAR wtab_msg.
          ENDLOOP.

          CONCATENATE 'DOCUMENTO ' o_alv-belnr INTO msg_alv SEPARATED BY space.
          CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              endpos_col   = 140
              endpos_row   = 20
              startpos_col = 60
              startpos_row = 15
              titletext    = msg_alv
            TABLES
              valuetab     = itab_msg
            EXCEPTIONS
              break_off    = 1
              OTHERS       = 2.
        ENDIF.
      ENDIF  .
    ELSE.
      READ TABLE o_alv INTO o_alv INDEX e_row_id-index.
      IF sy-subrc = 0.
        IF e_column_id = 'BELNR'.
          wl_ano = p_budat(4).
          SET PARAMETER ID 'BLN' FIELD o_alv-belnr.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          SET PARAMETER ID 'GJR' FIELD o_alv-budat(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF e_column_id = 'DOC_LCTO' AND o_alv-doc_lcto IS NOT INITIAL.
          SELECT SINGLE  zib_contabil_chv~mandt
                zib_contabil_chv~obj_key
                zib_contabil_chv~belnr
                zib_contabil_chv~bukrs
                zib_contabil_chv~gjahr
           FROM zib_contabil_chv
           INTO wa_zib_contabil_chv
           WHERE zib_contabil_chv~obj_key = o_alv-obj_key.
          SET PARAMETER ID 'BLN' FIELD wa_zib_contabil_chv-belnr.
          SET PARAMETER ID 'BUK' FIELD wa_zib_contabil_chv-bukrs.
          SET PARAMETER ID 'GJR' FIELD wa_zib_contabil_chv-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ELSEIF o_alv-status = icon_incomplete.
          SELECT *
             FROM zib_contabil_err
             INTO TABLE it_zib_contabil_err
             WHERE obj_key  = o_alv-obj_key.

          wtab_msg-name    = '------------------------MENSAGEM ERRO---------------------------------------'.
          APPEND wtab_msg TO itab_msg .
          CLEAR wtab_msg.
          LOOP AT it_zib_contabil_err INTO wa_zib_contabil_err.
            wtab_msg-name = wa_zib_contabil_err-message.
            APPEND wtab_msg TO itab_msg .
            CLEAR wtab_msg.
          ENDLOOP.

          CONCATENATE 'DOCUMENTO ' o_alv-belnr INTO msg_alv SEPARATED BY space.
          CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
            EXPORTING
              endpos_col   = 140
              endpos_row   = 20
              startpos_col = 60
              startpos_row = 15
              titletext    = msg_alv
            TABLES
              valuetab     = itab_msg
            EXCEPTIONS
              break_off    = 1
              OTHERS       = 2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "catch_hotspot

  METHOD handle_top_of_page .
    PERFORM event_top_of_page USING e_dyndoc_id
    table_index .
  ENDMETHOD.                    "HANDLE_TOP_OF_PAGE


ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


DATA:       event_receiver   TYPE REF TO lcl_event_receiver.


**************************************************************************
INITIALIZATION.
**************************************************************************

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = sy-datum
    IMPORTING
      last_day_of_month = p_budat2
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH TEXT-017 space space space. "'Erro ao determinar dia!'
  ENDIF.
  p_augdt = p_budat2.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
  DATA: variante     LIKE disvariant,
        def_variante LIKE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  wg_save = 'X'.
  wg_variant-report = wg_repname.
  wg_x_variant = wg_variant.

  IF ( NOT p_vari IS INITIAL ).
    wg_variant-variant = p_vari.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = wg_variant
      i_save        = wg_save
    IMPORTING
      es_variant    = wg_x_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH TEXT-018. "Não existe variante
*    STOP.
  ELSE.
    MOVE wg_x_variant-variant TO p_vari.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hkont-low.
  PERFORM f_buscar_conta.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_hkont-high.
  PERFORM f_buscar_conta.


AT SELECTION-SCREEN.

  p_budat2 = p_budat.
  p_augdt = p_budat.
  p_spmon = p_budat.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'SC1'.
      screen-active = '0'.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
  ENDLOOP.

*************Processamento****************************
START-OF-SELECTION.
  DATA: wg_index TYPE sy-tabix.
*CONSISTÊNCIAS

  IF p_v_lanc EQ 'X' AND p_hkont2 NE 'X'.
    PERFORM busca_zgl12_avm.
    EXIT.
  ENDIF.
  "ELSEIF P_HKONT2 EQ 'X'.
  "  EXIT.
  "ENDIF.

  PERFORM zf_valida_campos CHANGING sy-subrc.
  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE bukrs land1
    FROM t001
    INTO wa_t001
   WHERE bukrs = p_bukrs.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE land1 waers
    FROM t005
    INTO wa_t005
   WHERE land1 = wa_t001-land1.

*  Atribui Moeda Principal
  wg_cur_p = wa_t005-waers.


  IF p_v_lanc <> 'X'.
*Verifica campos da T001B
*Verifica se o período esta aberto
    SELECT frye1 frpe1 FROM t001b
      INTO TABLE t_t001b
     WHERE bukrs EQ p_bukrs
       AND frye1 EQ p_budat(4)
       AND frpe1 EQ p_budat+4(2).

  ENDIF.

**********************************************************************
*Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA
**********************************************************************

  DATA: dt_tvarvc TYPE tvarvc-low.
  SELECT SINGLE MAX( low ) AS dt
        FROM tvarvc
       INTO dt_tvarvc
        WHERE name ='MAGGI_DATA_ARCHIVE'.

  DATA(dt_arquive) = | { dt_tvarvc+6(2) }/{ dt_tvarvc+4(2) }/{ dt_tvarvc+0(4) }|.

  DATA(msg_dt_tvarvc) = 'Os documentos até'  && dt_arquive && ' foram arquivados.  Para consulta-los usar a transação FBL3N'.

  IF p_budat <= dt_tvarvc .
    MESSAGE msg_dt_tvarvc TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE TO TRANSACTION 'ZGL042'.
  ENDIF.

**********************************************************************

*  IF SY-SUBRC <> 0.
*Se não encontrou o mês está fechado!
*    MESSAGE I398(00) WITH TEXT-015  "Mês referente a Data da avaliação está
*                          TEXT-016. "fechado,é possível somente a visualização '.
*
**    LEAVE TO TRANSACTION 'ZGL042'.
*  ELSE.
*Trata Taxa de Câmbio
  PERFORM busca_tx_cambio.
*Trata Partidas em Aberto
  CASE 'X'.
    WHEN p_lifnr.
      PERFORM f_fornecedor.

      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_atual
       WHERE bukrs   EQ p_bukrs
         AND dt_aval EQ p_budat
         AND lifnr   IN s_lifnr
         AND hkont   IN s_kont
         AND lifnr   NE ' '
         AND estorno NE 'X'.

      DELETE t_zgl012_atual WHERE lifnr = space.

    WHEN p_kunnr.
      PERFORM f_cliente.

      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_atual
       WHERE bukrs EQ p_bukrs
         AND dt_aval EQ p_budat
         AND kunnr   IN s_kunnr
         AND hkont   IN s_dont
         AND kunnr   NE ' '
         AND estorno NE 'X'.

      DELETE t_zgl012_atual WHERE kunnr = space.

    WHEN p_hkont.
      PERFORM f_conta.

      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_atual
       WHERE bukrs EQ p_bukrs
         AND dt_aval EQ p_budat
         AND hkont   IN s_hkont
         AND lifnr   EQ ' '
         AND kunnr   EQ ' '
         AND estorno NE 'X'.

      DELETE t_zgl012_atual WHERE hkont = space.
      DELETE t_zgl012_atual WHERE ( kunnr NE space  OR  lifnr NE space ).
    WHEN p_hkont2.
      IF p_e_lanc IS INITIAL.
        PERFORM f_conta2.
      ENDIF.
  ENDCASE.

  IF p_bukrs = '0201' OR p_bukrs = '0202'.
    DELETE t_zgl012_atual WHERE waers = 'USD'.
  ENDIF.

  IF NOT t_zgl012_atual[] IS INITIAL AND p_hkont2 NE 'X'.
    "
    "substitui valores tabelas standard pelos valores ja criados
    LOOP AT t_zgl012_atual INTO wa_zgl012_aux.
      DELETE t_zgl012_avm WHERE bukrs     = wa_zgl012_aux-bukrs   AND
                                dt_aval   = wa_zgl012_aux-dt_aval AND
                                belnr     = wa_zgl012_aux-belnr   AND
                                buzei     = wa_zgl012_aux-buzei   AND
                                moeda_atu = wa_zgl012_aux-moeda_atu.

    ENDLOOP.

    APPEND LINES OF t_zgl012_atual TO t_zgl012_avm.

  ENDIF.

  IF t_zgl012_avm[] IS NOT INITIAL  AND p_hkont2 NE 'X'.
    SELECT *
      FROM zib_contabil_err
      INTO TABLE it_zib_contabil_err
       FOR ALL ENTRIES IN t_zgl012_avm
     WHERE obj_key = t_zgl012_avm-obj_key.

    SORT it_zib_contabil_err BY obj_key.

    SELECT zib_contabil_chv~mandt
           zib_contabil_chv~obj_key
           zib_contabil_chv~belnr
           zib_contabil_chv~bukrs
           zib_contabil_chv~gjahr
           zib_contabil~bldat
      FROM zib_contabil_chv
     INNER JOIN zib_contabil ON zib_contabil~obj_key EQ zib_contabil_chv~obj_key
      INTO TABLE it_zib_contabil_chv
       FOR ALL ENTRIES IN t_zgl012_avm
     WHERE zib_contabil_chv~obj_key = t_zgl012_avm-obj_key.

    SORT it_zib_contabil_chv BY obj_key.

  ENDIF.

  PERFORM f_alv.

*  ENDIF.
*************Subrotinas*******************************
*&---------------------------------------------------------------------*
*&      Form  BUSCA_ZGL12_AVM
*&---------------------------------------------------------------------*
*     Trata Fechamento Mês
*----------------------------------------------------------------------*
FORM busca_zgl12_avm .

  CASE 'X'.
    WHEN p_lifnr.
      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_avm
       WHERE bukrs   EQ p_bukrs
         AND dt_aval EQ p_budat
         AND lifnr   IN s_lifnr
         AND hkont   IN s_kont
         AND lifnr   NE ''.
    WHEN p_kunnr.
      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_avm
       WHERE bukrs   EQ p_bukrs
         AND dt_aval EQ p_budat
         AND kunnr   IN s_kunnr
         AND hkont   IN s_dont
         AND kunnr   NE ''.
    WHEN p_hkont.
      SELECT *
        FROM zgl012_avm
        INTO TABLE t_zgl012_avm
       WHERE bukrs   EQ p_bukrs
         AND dt_aval EQ p_budat
         AND hkont   IN s_hkont
         AND hkont   NE ''
         AND lifnr   EQ ''
         AND kunnr   EQ ''.
  ENDCASE.

  IF sy-subrc = 0.
    PERFORM: f_alv,
             atualiza_tela.

  ELSE.
    MESSAGE i000(su) WITH TEXT-019. "Registros não encontrados
*    STOP.
  ENDIF.

ENDFORM.                    " BUSCA_ZGL12_AVM
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TX_CAMBIO
*&---------------------------------------------------------------------*
*       Trata Taxa de Câmbio
*----------------------------------------------------------------------*
FORM busca_tx_cambio .
  RANGES: r_gdatu FOR tcurr-gdatu,
          r_fcurr FOR tcurr-fcurr.

  DATA: wl_date_aux  TYPE datum,
        wl_date_v1   TYPE datum,
        wl_input(10).

  MOVE 'IEQ' TO r_gdatu.

  wl_date_aux = p_budat.

  IF ( p_bukrs NE '0200' ) AND
     ( p_bukrs NE '0201' ) AND
     ( p_bukrs NE '0202' ).
    ADD 1 TO wl_date_aux.
  ENDIF.

  WRITE wl_date_aux TO wl_input.

*** PBI - 64926 - Inicio - CSB
  IF ( p_bukrs EQ '0203' ).
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
    DATA: lv_lastday TYPE sy-datum,
          lt_day_att TYPE STANDARD TABLE OF casdayattr.

    SELECT *
      FROM tvarvc
      INTO TABLE @DATA(t_varvc)
      WHERE name LIKE 'Z_WEEK_%'.

    IF sy-subrc IS INITIAL.
      LOOP AT t_varvc INTO DATA(lw_varvc).
        IF lw_varvc CA '5'.
          DATA(lv_days_5) = lw_varvc-low.
        ELSEIF lw_varvc CA '6'.
          DATA(lv_days_6) = lw_varvc-low.
        ELSEIF lw_varvc CA '7'.
          DATA(lv_days_7) = lw_varvc-low.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = p_budat
      IMPORTING
        last_day_of_month = lv_lastday
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    IF sy-subrc EQ 0.

      REFRESH lt_day_att[].
      CALL FUNCTION 'DAY_ATTRIBUTES_GET'
        EXPORTING
          factory_calendar           = 'BR'
          holiday_calendar           = 'BR'
          date_from                  = lv_lastday
          date_to                    = lv_lastday
          language                   = sy-langu
        TABLES
          day_attributes             = lt_day_att
        EXCEPTIONS
          factory_calendar_not_found = 1
          holiday_calendar_not_found = 2
          date_has_invalid_format    = 3
          date_inconsistency         = 4
          OTHERS                     = 5.

      IF sy-subrc EQ 0.
        READ TABLE lt_day_att INTO DATA(lw_day_att) INDEX 1.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
        CLEAR: wl_date_aux, wl_input.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
        CASE lw_day_att-weekday.
          WHEN 5.
            wl_date_v1 = p_budat + lv_days_5.
          WHEN 6.
            wl_date_v1 = p_budat + lv_days_6.
          WHEN 7.
            wl_date_v1 = p_budat + lv_days_7.
          WHEN OTHERS.
            wl_date_v1 = p_budat + 1.
        ENDCASE.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
        zcl_miro=>get_proximo_dia_util(
          EXPORTING
            i_data_base        = CONV #( wl_date_v1 )
            i_signum           = '+'
            i_ck_data_zles0145 = abap_true
          RECEIVING
            r_data             = wl_date_aux  "Retorno.
          EXCEPTIONS
            erro               = 1 ).

        WRITE wl_date_aux TO wl_input.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Início
      ENDIF.
    ENDIF.
*** RMNI - CS1030154 - Validação último dia do mês - 24.10.2022 - Fim
  ENDIF.
*** PBI - 64926 - Fim - CSB


  IF wl_input IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = wl_input
      IMPORTING
        output = r_gdatu-low.

    APPEND r_gdatu.
  ENDIF.

*BUSCA TAXA EM EUR
  SELECT kurst fcurr tcurr gdatu ukurs
    FROM tcurr
    INTO TABLE t_tcurr
   WHERE kurst = 'B'
     AND tcurr EQ wa_t005-waers
     AND gdatu IN r_gdatu.

  IF sy-subrc = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
    SORT t_tcurr BY gdatu ASCENDING fcurr ASCENDING .
*    IF P_BUKRS EQ '0004'.
*      C_USD = 'BRL'.
*    ELSE.
*      C_USD = 'USD'.
*    ENDIF.
    READ TABLE t_tcurr INTO wa_tcurr WITH KEY fcurr = c_usd BINARY SEARCH.
    IF sy-subrc = 0.
      xtx_usd = wa_tcurr-ukurs.
    ELSE.
      MESSAGE e398(00) WITH TEXT-020. "Erro ao encontrar taxa de conversão do Dólar
    ENDIF.

  ELSE.
    MESSAGE e398(00) WITH TEXT-020."Erro ao encontrar taxa de conversão do Dólar'
  ENDIF.

  IF p_bukrs = '0101'.
    SELECT kurst fcurr tcurr gdatu ukurs
      FROM tcurr
      INTO TABLE t_tcurr_g
     WHERE kurst = 'G'
       AND tcurr EQ wa_t005-waers
       AND gdatu IN r_gdatu.

    IF sy-subrc = 0.
*Como o campo é de data invertida para pegar oa mais atual a data GDATU tem que ser ordenada do menor pro maior
      SORT t_tcurr_g BY gdatu ASCENDING fcurr ASCENDING .

      READ TABLE t_tcurr_g INTO wa_tcurr WITH KEY fcurr = c_usd BINARY SEARCH.
      IF sy-subrc = 0.
        xtx_usd = wa_tcurr-ukurs.
      ELSE.
        MESSAGE e398(00) WITH TEXT-020. "Erro ao encontrar taxa de conversão do Dólar
      ENDIF.
    ELSE.
      MESSAGE e398(00) WITH TEXT-020."Erro ao encontrar taxa de conversão do Dólar'
    ENDIF.
  ENDIF.

*  IF P_BUKRS = '0004' OR P_BUKRS = '0201' OR P_BUKRS = '0200'.
  SELECT kurst fcurr tcurr gdatu ukurs
     FROM tcurr
     INTO TABLE t_tcurr_a
    WHERE kurst = 'B'
      AND tcurr EQ 'USD'
      AND gdatu IN r_gdatu.

  SORT t_tcurr_a BY gdatu ASCENDING fcurr ASCENDING.
*  ENDIF.

ENDFORM.                    " BUSCA_TX_CAMBIO
*&---------------------------------------------------------------------*
*&      Form  F_FORNECEDOR
*&---------------------------------------------------------------------*
*       Partidas em Aberto fornecedor
*----------------------------------------------------------------------*
FORM f_fornecedor .

  DATA: BEGIN OF t_bkpf0 OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf0.

  DATA: BEGIN OF t_bkpf0e OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf0e.

  DATA: vg_first TYPE datum.

  CONCATENATE p_budat+0(6) '01' INTO vg_first.


  DATA: BEGIN OF itl_bsik OCCURS 0,
          bukrs TYPE bsik-bukrs,
          lifnr TYPE bsik-lifnr,
          umskz TYPE bsik-umskz,
          augbl TYPE bsik-augbl,
          gjahr TYPE bsik-gjahr,
          belnr TYPE bsik-belnr,
          buzei TYPE bsik-buzei,
          budat TYPE bsik-budat,
          waers TYPE bsik-waers,
          blart TYPE blart,
          bschl TYPE bschl,
          shkzg TYPE shkzg,
          gsber TYPE gsber,
          dmbtr TYPE bsik-dmbtr,
          wrbtr TYPE bsik-wrbtr,
          hkont TYPE bsik-hkont,
          dmbe2 TYPE bsik-dmbe2,
          augdt TYPE bsik-augdt,
        END OF itl_bsik.

  DATA: BEGIN OF itl_bkpf OCCURS 0,
          bukrs TYPE bkpf-bukrs,
          belnr TYPE bkpf-belnr,
          gjahr TYPE bkpf-gjahr,
          stgrd TYPE bkpf-stgrd,
          stblg TYPE bkpf-stblg,
          stjah TYPE bkpf-stjah,
        END OF itl_bkpf.

  IF p_bukrs = '0004'.
    wa_t005-waers = 'XXX'. "pega todas as moedas, inclusive BRL
  ENDIF.
  "Em Aberto
  SELECT bukrs lifnr umskz belnr buzei budat waers blart bschl shkzg gsber dmbtr wrbtr hkont dmbe2 gjahr
    FROM bsik
    INTO TABLE t_bsik
    WHERE bukrs EQ p_bukrs
      AND budat LE p_budat
      AND lifnr IN s_lifnr
      AND hkont IN s_kont
      AND waers NE wa_t005-waers
      AND blart NE 'VC'
      AND anln1 EQ ''
      AND belnr IN s_belnr
      AND dmbe2 GE wg_dmbt2.

  IF sy-subrc IS INITIAL.
    SELECT * FROM bkpf INTO TABLE t_bkpf0
      FOR ALL ENTRIES IN t_bsik
      WHERE bukrs = t_bsik-bukrs AND
            belnr = t_bsik-belnr AND
            gjahr = t_bsik-gjahr AND
            bstat = 'S'.

    IF sy-subrc = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM bkpf
         INTO TABLE t_bkpf0e
          FOR ALL ENTRIES IN t_bkpf0
           WHERE bukrs EQ t_bkpf0-bukrs
             AND belnr EQ t_bkpf0-stblg
             AND gjahr EQ t_bkpf0-stjah.

      LOOP AT t_bkpf0.
        READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf0-bukrs
                                     belnr = t_bkpf0-stblg
                                     gjahr = t_bkpf0-stjah.
        IF sy-subrc = 0.
          IF  t_bkpf0e-budat LE p_budat.
            READ TABLE t_bsik INTO wa_bsik WITH KEY bukrs = t_bkpf0-bukrs
                                                             belnr = t_bkpf0-belnr
                                                             gjahr = t_bkpf0-gjahr.
            IF sy-subrc = 0.
              DELETE t_bsik INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE t_bsik INTO wa_bsik WITH KEY bukrs = t_bkpf0-bukrs
                                                  belnr = t_bkpf0-belnr
                                                  gjahr = t_bkpf0-gjahr.
          IF sy-subrc = 0.
            DELETE t_bsik INDEX sy-tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

*  "compensados depois
  SELECT bukrs lifnr umskz augbl gjahr belnr buzei budat
       waers blart bschl shkzg gsber dmbtr wrbtr hkont dmbe2 augdt
  FROM bsak
  INTO TABLE itl_bsik
  WHERE bukrs EQ p_bukrs
    AND budat LE p_budat
    AND augdt GE vg_first "P_BUDAT
    AND lifnr IN s_lifnr
    AND hkont IN s_kont
    AND waers NE wa_t005-waers
    AND blart NE 'VC'
    AND anln1 EQ ''
    AND belnr IN s_belnr
    AND dmbe2 GE wg_dmbt2.

  IF sy-subrc = 0.
    SELECT *
      FROM bkpf INTO TABLE t_bkpf0
      FOR ALL ENTRIES IN itl_bsik
      WHERE bukrs = itl_bsik-bukrs AND
            belnr = itl_bsik-belnr AND
            gjahr = itl_bsik-gjahr AND
            bstat = 'S'. "MEMO

    IF sy-subrc = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM bkpf
         INTO TABLE t_bkpf0e
          FOR ALL ENTRIES IN t_bkpf0
           WHERE bukrs EQ t_bkpf0-bukrs
             AND belnr EQ t_bkpf0-stblg
             AND gjahr EQ t_bkpf0-stjah.

      LOOP AT t_bkpf0.
        READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf0-bukrs
                                 belnr = t_bkpf0-stblg
                                 gjahr = t_bkpf0-stjah.
        IF sy-subrc = 0.
          IF  t_bkpf0e-budat LE p_budat.
            READ TABLE itl_bsik WITH KEY bukrs = t_bkpf0-bukrs
                                       belnr = t_bkpf0-belnr
                                       gjahr = t_bkpf0-gjahr.
            IF sy-subrc = 0.
              DELETE itl_bsik INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE itl_bsik WITH KEY bukrs = t_bkpf0-bukrs
                                       belnr = t_bkpf0-belnr
                                       gjahr = t_bkpf0-gjahr.
          IF sy-subrc = 0.
            DELETE itl_bsik INDEX sy-tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT itl_bsik.

      IF itl_bsik-belnr = itl_bsik-augbl.
        DELETE itl_bsik INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      IF itl_bsik-budat(6) = itl_bsik-augdt(6).
        DELETE itl_bsik INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING itl_bsik TO wa_bsik.
      IF itl_bsik-augdt LE p_budat.
        wa_bsik-st_rev = 'S'.
      ENDIF.

      APPEND wa_bsik TO t_bsik.
      CLEAR wa_bsik.

    ENDLOOP.
  ENDIF.

  DELETE t_bsik WHERE blart = 'VC'.

  IF p_bukrs = '0004'.
    wa_t005-waers = 'BRL'. "pega todas as moedas, inclusive BRL
  ENDIF.
  PERFORM: carrega_t_zgl_for.     "Carrega tabela do ALV

ENDFORM.                    " F_FORNECEDOR
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_FOR
*&---------------------------------------------------------------------*
*       Carrega tabela aux. fornecedor
*----------------------------------------------------------------------*
FORM carrega_t_zgl_for .
  DATA: w_tb    TYPE sy-tabix,
        w_loop  TYPE i,
        w_dmbtr TYPE zgl012_avm-dmbtr,
        w_moeda TYPE zgl012_avm-waers.

  DATA tabix TYPE sy-tabix.

  PERFORM ultima_avaliacao_for.  "Trata ültima avaliação

  DELETE t_bsik WHERE dmbe2 EQ '0.01'
                  OR  umskz  EQ 'F'.

  LOOP AT t_bsik INTO wa_bsik.
    wa_zgl012_avm-mandt   = sy-mandt.
    wa_zgl012_avm-bukrs   = wa_bsik-bukrs.
    wa_zgl012_avm-dt_aval = p_budat.
    wa_zgl012_avm-lifnr   = wa_bsik-lifnr.
    wa_zgl012_avm-belnr   = wa_bsik-belnr.
    wa_zgl012_avm-buzei   = wa_bsik-buzei.
    wa_zgl012_avm-budat   = wa_bsik-budat.
    wa_zgl012_avm-bschl   = wa_bsik-bschl.
    wa_zgl012_avm-waers   = wa_bsik-waers.
    wa_zgl012_avm-gsber   = wa_bsik-gsber.
    wa_zgl012_avm-st_rev  = wa_bsik-st_rev.
    wa_zgl012_avm-augbl   = wa_bsik-augbl.
    wa_zgl012_avm-augdt   = wa_bsik-augdt.

    IF wa_bsik-shkzg = 'S'.
      wa_zgl012_avm-dmbtr   = wa_bsik-dmbtr.
      wa_zgl012_avm-dmbe2   = wa_bsik-dmbe2.
      wa_zgl012_avm-wrbtr   = wa_bsik-wrbtr.
    ELSE.
      wa_zgl012_avm-dmbtr   = wa_bsik-dmbtr * ( -1 ).
      wa_zgl012_avm-dmbe2   = wa_bsik-dmbe2 * ( -1 ).
      wa_zgl012_avm-wrbtr   = wa_bsik-wrbtr * ( -1 ).
    ENDIF.

    IF p_bukrs = '0004' OR p_bukrs = '0201' OR p_bukrs = '0202' OR p_bukrs = '0037'. "Elimina moeda dolar
      IF wa_zgl012_avm-waers = c_usd.
        CONTINUE.
      ENDIF.
    ENDIF.


    IF wg_cur_p EQ 'PYG'.
      wa_zgl012_avm-dmbtr = wa_zgl012_avm-dmbtr * 100.
    ENDIF.

    IF wa_zgl012_avm-waers = c_usd.
      w_loop = 1.
    ELSE.
      w_loop = 2.
    ENDIF.

    CLEAR: w_dmbtr.
    DO w_loop TIMES.
      xtx_usd = 0.
      IF sy-index = 1.
        w_moeda = ''.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsik-hkont
            IMPORTING
              output = wa_bsik-hkont.
          IF wa_bsik-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsik-hkont
            IMPORTING
              output = wa_bsik-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsik-dmbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsik-wrbtr / wa_bsik-dmbtr.
          ENDIF.
        ELSE.
          IF wa_bsik-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsik-dmbtr / wa_bsik-wrbtr.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        w_moeda = c_usd.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = c_usd.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsik-hkont
            IMPORTING
              output = wa_bsik-hkont.
          IF wa_bsik-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = c_usd.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsik-hkont
            IMPORTING
              output = wa_bsik-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsik-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsik-dmbe2 / wa_bsik-wrbtr.
          ENDIF.
        ELSE.
          IF wa_bsik-dmbe2 NE 0.
            wa_zgl012_avm-kursf  = wa_bsik-wrbtr / wa_bsik-dmbe2.
          ENDIF.
        ENDIF.
      ENDIF.


      IF wa_zgl012_avm-tx_fech LT 0.
        MULTIPLY wa_zgl012_avm-tx_fech BY -1.
      ENDIF.

      IF sy-index = 1.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech.
        ENDIF.
        w_dmbtr = wa_zgl012_avm-vlr_atualizado.
        IF p_bukrs = '0201' OR p_bukrs = '0202' OR ( p_bukrs = '0004' AND wa_bsik-waers = 'BRL' ).
          IF p_bukrs = '0004'.
            w_dmbtr = wa_zgl012_avm-dmbtr. " valor em BRL para atualizar USD Somente neste caso
          ENDIF.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr  * wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 - wa_zgl012_avm-vlr_atualizado .
        ELSE.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr / wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 -   wa_zgl012_avm-vlr_atualizado.
        ENDIF.
      ENDIF.

      wa_zgl012_avm-hkont   = wa_bsik-hkont.
      wa_zgl012_avm-umskz   = wa_bsik-umskz.
*Grava valor acumulado do mês anterior.

      READ TABLE t_zgl012_aux INTO wa_zgl012_aux WITH KEY
                                   bukrs     =  wa_zgl012_avm-bukrs
                                   lifnr     =  wa_zgl012_avm-lifnr
                                   belnr     =  wa_zgl012_avm-belnr
                                   buzei     =  wa_zgl012_avm-buzei
                                   budat     =  wa_zgl012_avm-budat
                                   moeda_atu =  w_moeda.

*NAO UTILIZAR O BINARY SEARCH POIS COM O COMANDO DELETE ABAIXO PODE DESORDENAR
      IF sy-subrc = 0 .
        IF wa_zgl012_aux-doc_rev IS INITIAL.
          wa_zgl012_avm-vlr_acum_mes_ant = wa_zgl012_aux-vlr_acum_mes_atu.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_ant = 0.
        ENDIF.

      ELSE.
        wa_zgl012_avm-vlr_acum_mes_ant = 0.
      ENDIF.

      wa_zgl012_avm-vlr_variacao = wa_zgl012_avm-vlr_acum_mes_atu - wa_zgl012_avm-vlr_acum_mes_ant.
      wa_zgl012_avm-moeda_atu = w_moeda.
*Grava documentos compensados
      APPEND wa_zgl012_avm TO t_zgl012_avm.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " CARREGA_T_ZGL_FOR

*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_FOR
*&---------------------------------------------------------------------*
*       Última data gravada_
*----------------------------------------------------------------------*
FORM ultima_avaliacao_for.
*Define última data de avaliação gravada.
  DATA: l_udata TYPE datum,
        l_pdata TYPE datum.

  DATA: tl_bkpf  TYPE TABLE OF bkpf WITH HEADER LINE,
        tl_bkpfe TYPE TABLE OF bkpf WITH HEADER LINE.

  l_udata = p_budat.
  l_udata+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  l_udata = l_udata - 1. "transforma no último dia do mês anterior

  l_pdata = l_udata - 120.

  CHECK t_bsik[] IS NOT INITIAL.

  SELECT * INTO TABLE t_zgl012_aux FROM zgl012_avm
    FOR ALL ENTRIES IN t_bsik
    WHERE
      bukrs   EQ t_bsik-bukrs  AND
      belnr   EQ t_bsik-belnr  AND
      buzei   EQ t_bsik-buzei  AND
      dt_aval GE l_pdata       AND
      dt_aval LE l_udata       AND
      estorno NE 'X'           AND
      lifnr   IN  s_lifnr      AND
      hkont   IN  s_kont.

  IF sy-subrc = 0.
    SELECT *
      FROM bkpf
      INTO TABLE tl_bkpf
       FOR ALL ENTRIES IN t_zgl012_aux
        WHERE bukrs EQ t_zgl012_aux-bukrs
          AND belnr EQ t_zgl012_aux-belnr
          AND stblg NE space.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF tl_bkpf[] IS NOT INITIAL.
      SELECT *
         FROM bkpf
         INTO TABLE tl_bkpfe
          FOR ALL ENTRIES IN tl_bkpf
           WHERE bukrs EQ tl_bkpf-bukrs
             AND belnr EQ tl_bkpf-stblg
             AND gjahr EQ tl_bkpf-stjah .
    ENDIF.

    SORT t_zgl012_aux BY bukrs belnr.
    LOOP AT tl_bkpf.
      READ TABLE tl_bkpfe WITH KEY bukrs = tl_bkpf-bukrs
                                  belnr = tl_bkpf-stblg
                                  gjahr = tl_bkpf-stjah.
      IF sy-subrc = 0.
        IF  tl_bkpfe-budat LE p_budat.
          LOOP AT t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                               AND   bukrs EQ tl_bkpf-bukrs.
            t_zgl012_aux-dele = 'X'.
            MODIFY t_zgl012_aux INDEX sy-tabix TRANSPORTING dele.
          ENDLOOP.
        ENDIF.
      ELSE.
        DELETE t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                              AND bukrs EQ tl_bkpf-bukrs.
      ENDIF.

    ENDLOOP.

    SORT t_zgl012_aux BY bukrs belnr buzei dt_aval moeda_atu.
    DELETE ADJACENT DUPLICATES FROM t_zgl012_aux COMPARING bukrs belnr buzei dt_aval moeda_atu.
    SORT t_zgl012_aux BY bukrs lifnr belnr buzei budat moeda_atu ASCENDING   dt_aval  DESCENDING.
  ENDIF.
  CLEAR l_udata.
ENDFORM.                    " ULTIMA_AVALIACAO_FOR

*&---------------------------------------------------------------------*
*&      Form  F_CLIENTE
*&---------------------------------------------------------------------*
*       Partidas em Aberto Cliente
*----------------------------------------------------------------------*
FORM f_cliente .

  DATA: BEGIN OF t_bkpf00 OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf00.

  DATA: BEGIN OF t_bkpf0e OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf0e.


  DATA: BEGIN OF itl_bsid OCCURS 0,
          bukrs TYPE bsid-bukrs,
          kunnr TYPE bsid-kunnr,
          umskz TYPE bsid-umskz,
          augbl TYPE bsid-augbl,
          gjahr TYPE bsid-gjahr,
          belnr TYPE bsid-belnr,
          buzei TYPE bsid-buzei,
          budat TYPE bsid-budat,
          waers TYPE bsid-waers,
          blart TYPE blart,
          bschl TYPE bschl,
          shkzg TYPE shkzg,
          gsber TYPE gsber,
          dmbtr TYPE bsid-dmbtr,
          wrbtr TYPE bsid-wrbtr,
          hkont TYPE bsid-hkont,
          dmbe2 TYPE bsid-dmbe2,
          augdt TYPE bsid-augdt,
        END OF itl_bsid.

  DATA: BEGIN OF itl_bkpf OCCURS 0,
          bukrs TYPE bkpf-bukrs,
          belnr TYPE bkpf-belnr,
          gjahr TYPE bkpf-gjahr,
          stgrd TYPE bkpf-stgrd,
          stblg TYPE bkpf-stblg,
          stjah TYPE bkpf-stjah,
        END OF itl_bkpf.


  DATA: vg_first TYPE datum.

  CONCATENATE p_budat+0(6) '01' INTO vg_first.


  "Abertos
  SELECT bukrs kunnr umskz belnr buzei budat waers blart bschl shkzg gsber dmbtr wrbtr hkont dmbe2 gjahr
      FROM bsid
      INTO TABLE t_bsid
      WHERE bukrs EQ p_bukrs
        AND budat LE p_budat
        AND kunnr IN s_kunnr
        AND hkont IN s_dont
        AND waers NE wa_t005-waers
        AND blart NE 'VC'
        AND belnr IN s_belnr.

  IF sy-subrc = 0.

    SELECT * FROM bkpf INTO TABLE t_bkpf00
      FOR ALL ENTRIES IN t_bsid
      WHERE bukrs = t_bsid-bukrs AND
            belnr = t_bsid-belnr AND
            gjahr = t_bsid-gjahr AND
            bstat = 'S'.

    IF sy-subrc = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM bkpf
         INTO TABLE t_bkpf0e
          FOR ALL ENTRIES IN t_bkpf00
           WHERE bukrs EQ t_bkpf00-bukrs
             AND belnr EQ t_bkpf00-stblg
             AND gjahr EQ t_bkpf00-stjah.

      LOOP AT t_bkpf00.
        READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf00-bukrs
                                   belnr = t_bkpf00-stblg
                                   gjahr = t_bkpf00-stjah.
        IF sy-subrc = 0.
          IF  t_bkpf0e-budat LE p_budat.
            READ TABLE t_bsid INTO wa_bsid WITH KEY bukrs = t_bkpf00-bukrs
                                     belnr = t_bkpf00-belnr
                                     gjahr = t_bkpf00-gjahr.
            IF sy-subrc = 0.
              DELETE t_bsid INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE t_bsid INTO wa_bsid WITH KEY bukrs = t_bkpf00-bukrs
                                       belnr = t_bkpf00-belnr
                                       gjahr = t_bkpf00-gjahr.
          IF sy-subrc = 0.
            DELETE t_bsid INDEX sy-tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  "compensados depois da data
  SELECT bukrs kunnr umskz augbl gjahr belnr buzei budat waers blart bschl shkzg gsber dmbtr wrbtr hkont dmbe2 augdt
    FROM bsad
    INTO TABLE  itl_bsid
   WHERE bukrs EQ p_bukrs
     AND budat LE p_budat
     AND augdt GE vg_first
     AND kunnr IN s_kunnr
     AND hkont IN s_dont
     AND waers NE wa_t005-waers
     AND blart NE 'VC'
     AND belnr IN s_belnr
     AND dmbe2 GE wg_dmbt2.

  IF sy-subrc = 0.
    SELECT * FROM bkpf
      INTO TABLE t_bkpf00
       FOR ALL ENTRIES IN itl_bsid
     WHERE bukrs = itl_bsid-bukrs AND
           belnr = itl_bsid-belnr AND
           gjahr = itl_bsid-gjahr AND
           bstat = 'S'.

    IF sy-subrc = 0.
      "ALRS 13.11.2014 Estorno dentro do mês não leva
      SELECT *
         FROM bkpf
         INTO TABLE t_bkpf0e
          FOR ALL ENTRIES IN t_bkpf00
        WHERE bukrs EQ t_bkpf00-bukrs
          AND belnr EQ t_bkpf00-stblg
          AND gjahr EQ t_bkpf00-stjah.

      LOOP AT t_bkpf00.
        READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf00-bukrs
                                     belnr = t_bkpf00-stblg
                                     gjahr = t_bkpf00-stjah.
        IF sy-subrc = 0.
          IF  t_bkpf0e-budat LE p_budat.
            READ TABLE itl_bsid WITH KEY bukrs = t_bkpf00-bukrs
                               belnr = t_bkpf00-belnr
                               gjahr = t_bkpf00-gjahr.
            IF sy-subrc = 0.
              DELETE itl_bsid INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ELSE.
          READ TABLE itl_bsid WITH KEY bukrs = t_bkpf00-bukrs
                                       belnr = t_bkpf00-belnr
                                       gjahr = t_bkpf00-gjahr.
          IF sy-subrc = 0.
            DELETE itl_bsid INDEX sy-tabix.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.


  LOOP AT itl_bsid.
    IF itl_bsid-belnr = itl_bsid-augbl.
      DELETE itl_bsid INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    IF itl_bsid-budat(6) = itl_bsid-augdt(6).
      DELETE itl_bsid INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING itl_bsid TO wa_bsid.
    IF itl_bsid-augdt LE p_budat.
      wa_bsid-st_rev = 'S'.
    ENDIF.
    APPEND wa_bsid TO t_bsid.
    CLEAR wa_bsid.
  ENDLOOP.
  DELETE t_bsid WHERE blart = 'VC'.

  PERFORM: carrega_t_zgl_cli.     "Carrega tabela do ALV

ENDFORM.                    " F_CLIENTE
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_CLI
*&---------------------------------------------------------------------*
*       Carrega tabela aux. cliente
*----------------------------------------------------------------------*
FORM carrega_t_zgl_cli .

  DATA: w_loop  TYPE i,
        w_dmbtr TYPE zgl012_avm-dmbtr,
        w_moeda TYPE zgl012_avm-waers.

  PERFORM: ultima_avaliacao_cli.  "Trata ültima avaliação

  DELETE t_bsid WHERE dmbe2 EQ '0.01'
                   OR umskz  EQ 'F'.

  LOOP AT t_bsid INTO wa_bsid.
    wa_zgl012_avm-mandt   = sy-mandt.
    wa_zgl012_avm-bukrs   = wa_bsid-bukrs.
    wa_zgl012_avm-dt_aval = p_budat.
    wa_zgl012_avm-kunnr   = wa_bsid-kunnr.
    wa_zgl012_avm-belnr   = wa_bsid-belnr.
    wa_zgl012_avm-buzei   = wa_bsid-buzei.
    wa_zgl012_avm-budat   = wa_bsid-budat.
    wa_zgl012_avm-bschl   = wa_bsid-bschl.
    wa_zgl012_avm-waers   = wa_bsid-waers.
    wa_zgl012_avm-gsber   = wa_bsid-gsber.
    wa_zgl012_avm-st_rev  = wa_bsid-st_rev.
    wa_zgl012_avm-augbl   = wa_bsid-augbl.
    wa_zgl012_avm-augdt   = wa_bsid-augdt.

    IF wa_bsid-shkzg = 'S'.
      wa_zgl012_avm-dmbtr   = wa_bsid-dmbtr.
      wa_zgl012_avm-dmbe2   = wa_bsid-dmbe2.
      wa_zgl012_avm-wrbtr   = wa_bsid-wrbtr.
    ELSE.
      wa_zgl012_avm-dmbtr   = wa_bsid-dmbtr * ( -1 ).
      wa_zgl012_avm-dmbe2   = wa_bsid-dmbe2 * ( -1 ).
      wa_zgl012_avm-wrbtr   = wa_bsid-wrbtr * ( -1 ).
    ENDIF.

    IF p_bukrs = '0004' OR p_bukrs = '0201' OR p_bukrs = '0202' OR p_bukrs = '0037'. "Elimina moeda dolar
      IF wa_zgl012_avm-waers = c_usd.
        CONTINUE.
      ENDIF.
    ENDIF.


    IF wg_cur_p EQ 'PYG'.
      wa_zgl012_avm-dmbtr = wa_zgl012_avm-dmbtr * 100.
    ENDIF.

    IF wa_zgl012_avm-waers = c_usd.
      w_loop = 1.
    ELSE.
      w_loop = 2.
    ENDIF.

    DO w_loop TIMES.
      xtx_usd = 0.
      IF sy-index = 1.
        w_moeda = ''.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsid-hkont
            IMPORTING
              output = wa_bsid-hkont.
          IF wa_bsid-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsid-hkont
            IMPORTING
              output = wa_bsid-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsid-dmbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsid-wrbtr / wa_bsid-dmbtr.
          ENDIF.
        ELSE.
          IF wa_bsid-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsid-dmbtr / wa_bsid-wrbtr.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        w_moeda = c_usd.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = c_usd.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsid-hkont
            IMPORTING
              output = wa_bsid-hkont.
          IF wa_bsid-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = c_usd.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsid-hkont
            IMPORTING
              output = wa_bsid-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsid-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsid-dmbe2 / wa_bsid-wrbtr.
          ENDIF.
        ELSE.
          IF wa_bsid-dmbe2 NE 0.
            wa_zgl012_avm-kursf  = wa_bsid-wrbtr / wa_bsid-dmbe2.
          ENDIF.
        ENDIF.
      ENDIF.

      IF wa_zgl012_avm-tx_fech LT 0.
        MULTIPLY wa_zgl012_avm-tx_fech BY -1.
      ENDIF.

      IF sy-index = 1.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech.
        ENDIF.
        w_dmbtr = wa_zgl012_avm-vlr_atualizado.
        IF p_bukrs = '0201' OR p_bukrs = '0202'.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr  * wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 - wa_zgl012_avm-vlr_atualizado .
        ELSE.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr / wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 -   wa_zgl012_avm-vlr_atualizado.
        ENDIF.
      ENDIF.

      wa_zgl012_avm-hkont   = wa_bsid-hkont.
      wa_zgl012_avm-umskz   = wa_bsid-umskz.
*Grava valor acumulado do mês anterior.
      READ TABLE t_zgl012_aux INTO wa_zgl012_aux WITH KEY bukrs   =  wa_zgl012_avm-bukrs
                                                          kunnr   =  wa_zgl012_avm-kunnr
                                                          belnr   =  wa_zgl012_avm-belnr
                                                          buzei   =  wa_zgl012_avm-buzei
                                                          budat   =  wa_zgl012_avm-budat
                                                          moeda_atu =  w_moeda.
      IF sy-subrc = 0 .
        IF wa_zgl012_aux-doc_rev IS INITIAL.
          wa_zgl012_avm-vlr_acum_mes_ant = wa_zgl012_aux-vlr_acum_mes_atu.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_ant = 0.
        ENDIF.
      ELSE.
        wa_zgl012_avm-vlr_acum_mes_ant = 0.
      ENDIF.

      wa_zgl012_avm-vlr_variacao = wa_zgl012_avm-vlr_acum_mes_atu - wa_zgl012_avm-vlr_acum_mes_ant.
      wa_zgl012_avm-moeda_atu = w_moeda.
*Grava documentos compensados
      APPEND wa_zgl012_avm TO t_zgl012_avm.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " CARREGA_T_ZGL_CLI
*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_CLI
*&---------------------------------------------------------------------*
*       Última data gravada
*----------------------------------------------------------------------*
FORM ultima_avaliacao_cli .
*Define última data de avaliação gravada.
  DATA: l_udata TYPE datum,
        l_pdata TYPE datum.

  DATA: tl_bkpf  TYPE TABLE OF bkpf WITH HEADER LINE,
        tl_bkpfe TYPE TABLE OF bkpf WITH HEADER LINE.

  l_udata = p_budat.
  l_udata+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  l_udata = l_udata - 1. "transforma no último dia do mês anterior

  l_pdata = l_udata - 120.


  CHECK t_bsid[] IS NOT INITIAL.

  SELECT * INTO TABLE t_zgl012_aux
    FROM zgl012_avm
    FOR ALL ENTRIES IN t_bsid
   WHERE
      bukrs   EQ t_bsid-bukrs  AND
      belnr   EQ t_bsid-belnr  AND
      buzei   EQ t_bsid-buzei  AND
      dt_aval GE l_pdata       AND
      dt_aval LE  l_udata      AND
      estorno NE 'X'           AND
      kunnr   IN s_kunnr       AND
      hkont   IN s_dont.

  IF sy-subrc = 0.
    SELECT *
      FROM bkpf
      INTO TABLE tl_bkpf
       FOR ALL ENTRIES IN t_zgl012_aux
     WHERE bukrs EQ t_zgl012_aux-bukrs
       AND belnr EQ t_zgl012_aux-belnr
       AND stblg NE space.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF tl_bkpf[] IS NOT INITIAL.
      SELECT *
        FROM bkpf
        INTO TABLE tl_bkpfe
         FOR ALL ENTRIES IN tl_bkpf
       WHERE bukrs EQ tl_bkpf-bukrs
         AND belnr EQ tl_bkpf-stblg
         AND gjahr EQ tl_bkpf-stjah .

    ENDIF.

    LOOP AT tl_bkpf.
      READ TABLE tl_bkpfe WITH KEY bukrs = tl_bkpf-bukrs
                                   belnr = tl_bkpf-stblg
                                   gjahr = tl_bkpf-stjah.
      IF sy-subrc = 0.
        IF  tl_bkpfe-budat LE p_budat.
          LOOP AT t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                               AND   bukrs EQ tl_bkpf-bukrs.
            t_zgl012_aux-dele = 'X'.
            MODIFY t_zgl012_aux INDEX sy-tabix TRANSPORTING dele.

          ENDLOOP.
        ENDIF.
      ELSE.
        DELETE t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                              AND bukrs EQ tl_bkpf-bukrs.

      ENDIF.

    ENDLOOP.

    SORT t_zgl012_aux BY bukrs belnr buzei dt_aval moeda_atu .
    DELETE ADJACENT DUPLICATES FROM t_zgl012_aux COMPARING bukrs belnr buzei dt_aval moeda_atu.
    SORT t_zgl012_aux BY bukrs kunnr belnr buzei budat moeda_atu ASCENDING   dt_aval  DESCENDING.

  ENDIF.

  CLEAR l_udata.

ENDFORM.                    " ULTIMA_AVALIACAO_CLI

*&---------------------------------------------------------------------*
*&      Form  F_CONTA
*&---------------------------------------------------------------------*
*       Partidas em Aberto Razão
*----------------------------------------------------------------------*
FORM f_conta .

  DATA: BEGIN OF t_bkpf000 OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf000.

  DATA: BEGIN OF t_bkpf0e OCCURS 0.
          INCLUDE STRUCTURE bkpf.
  DATA: END OF t_bkpf0e.

  DATA: BEGIN OF itl_bsis OCCURS 0,
          bukrs TYPE bsis-bukrs,
          hkont TYPE bsis-hkont,
          augbl TYPE bsis-augbl,
          gjahr TYPE bsis-gjahr,
          belnr TYPE bsis-belnr,
          buzei TYPE bsis-buzei,
          budat TYPE bsis-budat,
          waers TYPE bsis-waers,
          blart TYPE blart,
          bschl TYPE bschl,
          shkzg TYPE shkzg,
          gsber TYPE gsber,
          dmbtr TYPE bsis-dmbtr,
          wrbtr TYPE bsis-wrbtr,
          dmbe2 TYPE bsis-dmbe2,
          augdt LIKE bsis-augdt,
        END OF itl_bsis.


  DATA: BEGIN OF itl_bkpf OCCURS 0,
          bukrs TYPE bkpf-bukrs,
          belnr TYPE bkpf-belnr,
          gjahr TYPE bkpf-gjahr,
          stgrd TYPE bkpf-stgrd,
          stblg TYPE bkpf-stblg,
          stjah TYPE bkpf-stjah,
        END OF itl_bkpf.


  DATA: vg_first TYPE datum.

  CONCATENATE p_budat+0(6) '01' INTO vg_first.

  "Abertos
  SELECT bukrs saknr xopvw             "#EC CI_DB_OPERATION_OK[2431747]
    FROM skb1
    INTO TABLE t_skb1
   WHERE bukrs = p_bukrs
     AND saknr IN s_hkont
     AND xopvw EQ 'X'.
  IF sy-subrc = 0.
    SELECT bukrs hkont belnr buzei budat waers blart bschl shkzg gsber dmbtr wrbtr dmbe2 gjahr
      INTO TABLE t_bsis
      FROM bsis
       FOR ALL ENTRIES IN t_skb1
      WHERE bukrs EQ t_skb1-bukrs
        AND hkont EQ t_skb1-saknr
        AND budat LE p_budat
        AND waers NE wa_t005-waers
        AND blart NE 'VC'
        AND belnr IN s_belnr.

    IF sy-subrc = 0.
      SELECT *
        FROM bkpf
        INTO TABLE t_bkpf000
         FOR ALL ENTRIES IN t_bsis
       WHERE bukrs = t_bsis-bukrs AND
             belnr = t_bsis-belnr AND
             gjahr = t_bsis-gjahr AND
             bstat = 'S'.

      IF sy-subrc = 0.
        "ALRS 13.11.2014 Estorno dentro do mês não leva
        SELECT *
           FROM bkpf
           INTO TABLE t_bkpf0e
            FOR ALL ENTRIES IN t_bkpf000
             WHERE bukrs EQ t_bkpf000-bukrs
               AND belnr EQ t_bkpf000-stblg
               AND gjahr EQ t_bkpf000-stjah.

        LOOP AT t_bkpf000.
          READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf000-bukrs
                                       belnr = t_bkpf000-stblg
                                       gjahr = t_bkpf000-stjah.
          IF sy-subrc = 0.
            IF  t_bkpf0e-budat LE p_budat.
              READ TABLE t_bsis INTO wa_bsis WITH KEY bukrs = t_bkpf000-bukrs
                                                    belnr = t_bkpf000-belnr
                                                    gjahr = t_bkpf000-gjahr.
              IF sy-subrc = 0.
                DELETE t_bsis INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE t_bsis INTO wa_bsis WITH KEY bukrs = t_bkpf000-bukrs
                                                    belnr = t_bkpf000-belnr
                                                    gjahr = t_bkpf000-gjahr.
            IF sy-subrc = 0.
              DELETE t_bsis INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "compensados no mês
    IF t_skb1[] IS NOT INITIAL.
      SELECT bukrs hkont augbl gjahr belnr buzei budat waers
             blart bschl shkzg gsber dmbtr wrbtr dmbe2 augdt
        FROM bsas
        INTO TABLE itl_bsis
        FOR ALL ENTRIES IN t_skb1
        WHERE bukrs EQ t_skb1-bukrs
          AND hkont EQ t_skb1-saknr
          AND budat LE p_budat
          AND waers NE wa_t005-waers
          AND blart NE 'VC'
          AND augdt GE vg_first
          AND belnr IN s_belnr.


    ENDIF.

    IF NOT itl_bsis[] IS INITIAL.
      SELECT * FROM bkpf INTO TABLE t_bkpf000
        FOR ALL ENTRIES IN itl_bsis
        WHERE bukrs = itl_bsis-bukrs AND
              belnr = itl_bsis-belnr AND
              gjahr = itl_bsis-gjahr AND
              bstat = 'S'.

      IF sy-subrc = 0.
        "ALRS 13.11.2014 Estorno dentro do mês não leva
        SELECT *
           FROM bkpf
           INTO TABLE t_bkpf0e
            FOR ALL ENTRIES IN t_bkpf000
             WHERE bukrs EQ t_bkpf000-bukrs
               AND belnr EQ t_bkpf000-stblg
               AND gjahr EQ t_bkpf000-stjah.

        LOOP AT t_bkpf000.
          READ TABLE t_bkpf0e WITH KEY bukrs = t_bkpf000-bukrs
                                       belnr = t_bkpf000-stblg
                                       gjahr = t_bkpf000-stjah.
          IF sy-subrc = 0.
            IF  t_bkpf0e-budat LE p_budat.
              READ TABLE itl_bsis WITH KEY bukrs = t_bkpf000-bukrs
                                           belnr = t_bkpf000-belnr
                                           gjahr = t_bkpf000-gjahr.
              IF sy-subrc = 0.
                DELETE itl_bsis INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE itl_bsis WITH KEY bukrs = t_bkpf000-bukrs
                                         belnr = t_bkpf000-belnr
                                         gjahr = t_bkpf000-gjahr.
            IF sy-subrc = 0.
              DELETE itl_bsis INDEX sy-tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.

    LOOP AT itl_bsis.
      IF itl_bsis-budat(6) = itl_bsis-augdt(6).
        DELETE itl_bsis INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      IF itl_bsis-belnr = itl_bsis-augbl.
        DELETE itl_bsis INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING itl_bsis TO wa_bsis.
      IF itl_bsis-augdt LE p_budat.
        wa_bsis-st_rev = 'S'.
      ENDIF.
      APPEND wa_bsis TO t_bsis.
      CLEAR wa_bsis.

    ENDLOOP.

    DELETE t_bsis WHERE blart = 'VC'.

    PERFORM: carrega_t_zgl_raz.     "Carrega tabela do ALV

  ENDIF.
ENDFORM.                    " F_CONTA
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T_ZGL_RAZ
*&---------------------------------------------------------------------*
*       Carrega tabela aux. Razão
*----------------------------------------------------------------------*
FORM carrega_t_zgl_raz .

  DATA: w_loop  TYPE i,
        w_dmbtr TYPE zgl012_avm-dmbtr,
        w_moeda TYPE zgl012_avm-waers.


  PERFORM: ultima_avaliacao_raz.

  DELETE t_bsis WHERE dmbe2 EQ '0.01'.

  LOOP AT t_bsis INTO wa_bsis.
    wa_zgl012_avm-mandt   = sy-mandt.
    wa_zgl012_avm-bukrs   = wa_bsis-bukrs.
    wa_zgl012_avm-dt_aval = p_budat.
    wa_zgl012_avm-hkont   = wa_bsis-hkont.
    wa_zgl012_avm-belnr   = wa_bsis-belnr.
    wa_zgl012_avm-buzei   = wa_bsis-buzei.
    wa_zgl012_avm-budat   = wa_bsis-budat.
    wa_zgl012_avm-bschl   = wa_bsis-bschl.
    wa_zgl012_avm-waers   = wa_bsis-waers.
    wa_zgl012_avm-gsber   = wa_bsis-gsber.
    wa_zgl012_avm-st_rev  = wa_bsis-st_rev.
    wa_zgl012_avm-augbl   = wa_bsis-augbl.
    wa_zgl012_avm-augdt   = wa_bsis-augdt.

    IF wa_bsis-shkzg = 'S'.
      wa_zgl012_avm-dmbtr   = wa_bsis-dmbtr.
      wa_zgl012_avm-dmbe2   = wa_bsis-dmbe2.
      wa_zgl012_avm-wrbtr   = wa_bsis-wrbtr.
    ELSE.
      wa_zgl012_avm-dmbtr   = wa_bsis-dmbtr * ( -1 ).
      wa_zgl012_avm-dmbe2   = wa_bsis-dmbe2 * ( -1 ).
      wa_zgl012_avm-wrbtr   = wa_bsis-wrbtr * ( -1 ).
    ENDIF.

    IF p_bukrs = '0004' OR p_bukrs = '0201' OR p_bukrs = '0202' OR p_bukrs = '0037'.. "Elimina moeda dolar
      IF wa_zgl012_avm-waers = c_usd.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF wg_cur_p EQ 'PYG'.
      wa_zgl012_avm-dmbtr = wa_zgl012_avm-dmbtr * 100.
    ENDIF.

    IF wa_zgl012_avm-waers = c_usd.
      w_loop = 1.
    ELSE.
      w_loop = 2.
    ENDIF.

    DO w_loop TIMES.
      xtx_usd = 0.
      IF sy-index = 1.
        w_moeda = ''.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsis-hkont
            IMPORTING
              output = wa_bsis-hkont.
          IF wa_bsis-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = wa_zgl012_avm-waers.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsis-hkont
            IMPORTING
              output = wa_bsis-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsis-dmbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsis-wrbtr / wa_bsis-dmbtr.
          ENDIF.
        ELSE.
          IF wa_bsis-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsis-dmbtr / wa_bsis-wrbtr.
          ENDIF.
        ENDIF.
      ELSE. "ler o USD
        w_moeda = c_usd.
        READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = c_usd.
        IF p_bukrs = '0101'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_bsis-hkont
            IMPORTING
              output = wa_bsis-hkont.
          IF wa_bsis-hkont+0(1) = '1'. "ativos (compra)
            READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = c_usd.
          ENDIF.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_bsis-hkont
            IMPORTING
              output = wa_bsis-hkont.
        ENDIF.
        IF sy-subrc = 0.
          xtx_usd = wa_tcurr-ukurs.
          wa_zgl012_avm-tx_fech  = xtx_usd.
        ENDIF.

        IF xtx_usd LT 0.
          IF wa_bsis-wrbtr NE 0.
            wa_zgl012_avm-kursf   = wa_bsis-dmbe2 / wa_bsis-wrbtr.
          ENDIF.
        ELSE.
          IF wa_bsis-dmbe2 NE 0.
            wa_zgl012_avm-kursf  = wa_bsis-wrbtr / wa_bsis-dmbe2.
          ENDIF.
        ENDIF.
      ENDIF.
      IF xtx_usd LT 0.
        MULTIPLY wa_zgl012_avm-tx_fech BY -1.
      ENDIF.

      IF sy-index = 1.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr / wa_zgl012_avm-tx_fech.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbtr - ( wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech ) .
          wa_zgl012_avm-vlr_atualizado    = wa_zgl012_avm-wrbtr * wa_zgl012_avm-tx_fech.
        ENDIF.
        w_dmbtr = wa_zgl012_avm-vlr_atualizado.
        IF p_bukrs = '0201' OR p_bukrs = '0202'.
          CONTINUE.
        ENDIF.
      ELSE. "USD
*        IF wa_zgl012_avm-bukrs = '0201'.
*          MULTIPLY xtx_usd BY -1.
*        ENDIF.
        IF xtx_usd LT 0.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr  * wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 - wa_zgl012_avm-vlr_atualizado .
        ELSE.
          wa_zgl012_avm-vlr_atualizado    =  w_dmbtr / wa_zgl012_avm-tx_fech.
          wa_zgl012_avm-vlr_acum_mes_atu  = wa_zgl012_avm-dmbe2 -   wa_zgl012_avm-vlr_atualizado.
        ENDIF.
      ENDIF.

*Grava valor acumulado do mês anterior.
      READ TABLE t_zgl012_aux INTO wa_zgl012_aux WITH KEY bukrs   =  wa_zgl012_avm-bukrs
                                                          hkont   =  wa_zgl012_avm-hkont
                                                          belnr   =  wa_zgl012_avm-belnr
                                                          buzei   =  wa_zgl012_avm-buzei
                                                          budat   =  wa_zgl012_avm-budat
                                                          moeda_atu =  w_moeda.
      IF sy-subrc = 0 .
        IF wa_zgl012_aux-doc_rev IS INITIAL.
          wa_zgl012_avm-vlr_acum_mes_ant = wa_zgl012_aux-vlr_acum_mes_atu.
        ELSE.
          wa_zgl012_avm-vlr_acum_mes_ant = 0.
        ENDIF.
      ELSE.
        wa_zgl012_avm-vlr_acum_mes_ant = 0.
      ENDIF.

      wa_zgl012_avm-vlr_variacao = wa_zgl012_avm-vlr_acum_mes_atu - wa_zgl012_avm-vlr_acum_mes_ant.
      wa_zgl012_avm-moeda_atu = w_moeda.
*Grava documentos compensados
      IF NOT wa_zgl012_avm-budat+4(4) = wa_zgl012_avm-augdt+4(4).
        APPEND wa_zgl012_avm TO t_zgl012_avm.
      ENDIF.
    ENDDO.
  ENDLOOP.


ENDFORM.                    " CARREGA_T_ZGL_RAZ
*&---------------------------------------------------------------------*
*&      Form  ULTIMA_AVALIACAO_RAZ
*&---------------------------------------------------------------------*
*       Última data gravada
*----------------------------------------------------------------------*
FORM ultima_avaliacao_raz .
*Define última data de avaliação gravada.
  DATA: l_udata TYPE datum,
        l_pdata TYPE datum.

  DATA: tl_bkpf  TYPE TABLE OF bkpf WITH HEADER LINE,
        tl_bkpfe TYPE TABLE OF bkpf WITH HEADER LINE.

  l_udata = p_budat.
  l_udata+6(2) = '01'.   "tranforma em primeiro dia do mês corrente
  l_udata = l_udata - 1. "transforma no último dia do mês anterior

  l_pdata = l_udata - 120.

  CHECK t_bsis[] IS NOT INITIAL.

  SELECT * INTO TABLE t_zgl012_aux  FROM zgl012_avm
    FOR ALL ENTRIES IN t_bsis
    WHERE
      bukrs   EQ t_bsis-bukrs  AND
      belnr   EQ t_bsis-belnr  AND
      buzei   EQ t_bsis-buzei  AND
      dt_aval GE l_pdata       AND
      dt_aval LE l_udata       AND
      estorno NE 'X'           AND
      hkont   IN s_hkont .

  IF sy-subrc = 0.
    SELECT *
      FROM bkpf
      INTO TABLE tl_bkpf
       FOR ALL ENTRIES IN t_zgl012_aux
     WHERE bukrs EQ t_zgl012_aux-bukrs
       AND belnr EQ t_zgl012_aux-belnr
       AND stblg NE space.

    "ALRS 13.11.2014 Estorno dentro do mês não leva
    IF tl_bkpf[] IS NOT INITIAL.
      SELECT *
        FROM bkpf
        INTO TABLE tl_bkpfe
         FOR ALL ENTRIES IN tl_bkpf
       WHERE bukrs EQ tl_bkpf-bukrs
         AND belnr EQ tl_bkpf-stblg
         AND gjahr EQ tl_bkpf-stjah .

    ENDIF.

    LOOP AT tl_bkpf.
      READ TABLE tl_bkpfe WITH KEY bukrs = tl_bkpf-bukrs
                                   belnr = tl_bkpf-stblg
                                   gjahr = tl_bkpf-stjah.
      IF sy-subrc = 0.
        IF  tl_bkpfe-budat LE p_budat.
          LOOP AT t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                               AND   bukrs EQ tl_bkpf-bukrs.
            t_zgl012_aux-dele = 'X'.

            MODIFY t_zgl012_aux INDEX sy-tabix TRANSPORTING dele.

          ENDLOOP.

        ENDIF.

      ELSE.
        DELETE t_zgl012_aux WHERE belnr EQ tl_bkpf-belnr
                              AND bukrs EQ tl_bkpf-bukrs.
      ENDIF.

    ENDLOOP.

    SORT t_zgl012_aux BY bukrs belnr buzei dt_aval moeda_atu.
    DELETE ADJACENT DUPLICATES FROM t_zgl012_aux COMPARING bukrs belnr buzei dt_aval moeda_atu.

    SORT t_zgl012_aux BY bukrs hkont belnr buzei budat moeda_atu ASCENDING dt_aval DESCENDING.
  ENDIF.

  CLEAR l_udata.

ENDFORM.                    " ULTIMA_AVALIACAO_RAZ

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*      Apresentação do Relatório
*----------------------------------------------------------------------*
FORM f_alv .
  IF p_hkont2 = 'X'.
    PERFORM:
             f_alv_fieldcat2.
    CALL SCREEN 9002.
  ELSE.
    PERFORM:
             f_alv_fieldcat,
             f_alv_transf,
             f_alv_imprime,
             atualiza_tela .
  ENDIF.
ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_ALV_HEADER
*&---------------------------------------------------------------------*
*       Cabeçalho
*----------------------------------------------------------------------*
FORM zf_alv_header  USING p_tipo.
  DATA: wl_data(10),
        wl_hora(8),
        wl_linha(80),
        wl_text      TYPE sdydo_text_element,
        wl_dolar(15),
        wl_euro(15),
        wl_butxt     TYPE butxt,
        wl_cont      TYPE sy-tabix,
        wl_line      TYPE sy-tabix,
        wl_div       TYPE sy-tabix.

  IF p_tipo = '1'.
    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = TEXT-010
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>extra_large
        sap_color    = cl_dd_area=>list_heading_int.

    SELECT SINGLE butxt FROM t001
      INTO wl_butxt
      WHERE bukrs = p_bukrs.

    IF sy-subrc = 0.
      MOVE wl_butxt TO wl_linha.
    ELSE.
      MOVE 'N/a' TO wl_linha.
    ENDIF.

    CONCATENATE  TEXT-009 p_bukrs ' - ' wl_linha INTO wl_linha SEPARATED BY space.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.

    WRITE sy-uzeit TO wl_hora.
    WRITE sy-datum TO wl_data.

    CONCATENATE TEXT-011 wl_data TEXT-012 wl_hora INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.

    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
        sap_fontsize = cl_dd_area=>list_normal.

    WRITE p_budat TO wl_data.
    CONCATENATE TEXT-013 wl_data INTO wl_linha.
    wl_text = wl_linha.

    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.


    wl_cont = 0.
    CLEAR  wl_linha.
    IF p_bukrs = '0004' OR p_bukrs = '0201' OR p_bukrs = '0202' OR p_bukrs = '0037'.
      wl_line = lines( t_tcurr_a ).
      SORT t_tcurr_a BY gdatu ASCENDING fcurr DESCENDING .
      LOOP AT t_tcurr_a INTO wa_tcurr.
        wl_cont = sy-tabix.
        xtx_usd = wa_tcurr-ukurs.
        WRITE xtx_usd  TO wl_dolar.
        CONDENSE wl_dolar NO-GAPS.

        CONCATENATE  wl_linha TEXT-014 wa_tcurr-fcurr ':' wl_dolar INTO  wl_linha SEPARATED BY space.

        wl_div  = wl_cont MOD 3.
        IF wl_div EQ 0 OR wl_line EQ wl_cont.
          wl_text = wl_linha.

          CALL METHOD obj_dyndoc_id->new_line.

          CALL METHOD obj_dyndoc_id->add_text
            EXPORTING
              text         = wl_text
              sap_fontsize = cl_dd_area=>list_normal.

          CLEAR  wl_linha.
        ENDIF.
      ENDLOOP.
    ELSE.
      wl_line = lines( t_tcurr ).
      SORT t_tcurr BY gdatu ASCENDING fcurr DESCENDING .
      LOOP AT t_tcurr INTO wa_tcurr.
        wl_cont = sy-tabix.
        xtx_usd = wa_tcurr-ukurs.
        WRITE xtx_usd  TO wl_dolar.
        CONDENSE wl_dolar NO-GAPS.

        CONCATENATE  wl_linha TEXT-014 wa_tcurr-fcurr ':' wl_dolar INTO  wl_linha SEPARATED BY space.

        wl_div  = wl_cont MOD 3.
        IF wl_div EQ 0 OR wl_line EQ wl_cont.
          wl_text = wl_linha.

          CALL METHOD obj_dyndoc_id->new_line.

          CALL METHOD obj_dyndoc_id->add_text
            EXPORTING
              text         = wl_text
              sap_fontsize = cl_dd_area=>list_normal.

          CLEAR  wl_linha.
        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT t_tcurr BY gdatu ASCENDING fcurr ASCENDING .
  ELSE.
    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.

  ENDIF.

ENDFORM.                    " F_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       Características dos Campos
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .
  DATA i TYPE i.
  wa_afield-tabname     = 'O_ALV'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TIPO'.
  wa_afield-scrtext_s = TEXT-a01.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 10.
  wa_afield-key           = 'X'.
  APPEND wa_afield TO it_fieldcat.

*STATUS
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'STATUS'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot  = 'X'.
  wa_afield-scrtext_s = TEXT-a02.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 06.
  wa_afield-key           = 'X'.
  APPEND wa_afield TO it_fieldcat.
  CLEAR wa_afield-icon.

*Código
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CODIGO'.
  wa_afield-scrtext_s = TEXT-a03.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-ref_field = 'KUNNR'.
  wa_afield-ref_table = 'BSID'.
  wa_afield-key           = ''.
  wa_afield-key           = 'X'.
  APPEND wa_afield TO it_fieldcat.
  CLEAR:   wa_afield-ref_field ,  wa_afield-ref_table.

*Descrição
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DESCRICAO'.
  wa_afield-scrtext_s = TEXT-a04.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 25.
  wa_afield-key           = 'X'.
  APPEND wa_afield TO it_fieldcat.

*NR DOC
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = TEXT-a05.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-hotspot   = 'X'.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 12.
  wa_afield-key           = 'X'.
  APPEND wa_afield TO it_fieldcat.
  CLEAR wa_afield-hotspot.

*Chave de lançamento
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BSCHL'.
  wa_afield-scrtext_s = TEXT-a06.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 08.
  APPEND wa_afield TO it_fieldcat.

*CONTA
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'HKONT'.
  wa_afield-scrtext_s = TEXT-a07.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-ref_field = 'HKONT'.
  wa_afield-ref_table = 'BSIS'.
  wa_afield-key           = ''.

  APPEND wa_afield TO it_fieldcat.
  CLEAR: wa_afield-ref_field, wa_afield-ref_table.
*NRO. DOC.
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'UMSKZ'.
  wa_afield-scrtext_s =  TEXT-a08.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 08.
  APPEND wa_afield TO it_fieldcat.

*DT lANCAMENTO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUDAT'.
  wa_afield-scrtext_s = TEXT-a09.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*Moeda documento
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WAERS'.
  wa_afield-scrtext_s = TEXT-a10.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ' '.
  wa_afield-do_sum        = ''.
  wa_afield-outputlen     = 08.
  APPEND wa_afield TO it_fieldcat.

*moeda de atualização
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOEDA_ATU'.
  wa_afield-scrtext_s = TEXT-a38.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  APPEND wa_afield TO it_fieldcat.

*VALOR DOC
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WRBTR'.
  wa_afield-scrtext_m = TEXT-a16.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-do_sum        = ''.
  wa_afield-outputlen     = 18.
  APPEND wa_afield TO it_fieldcat.

*TX. CÂMBIO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KURSF'.
  wa_afield-scrtext_s = TEXT-a17.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
*  WA_AFIELD-REF_FIELDNAME = ''.
*  WA_AFIELD-REF_TABNAME   = ''.
  wa_afield-key           = ' '.
  wa_afield-do_sum        = ''.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*Valor interno
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DMBTR'.
  wa_afield-scrtext_m = TEXT-a14.

  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key       = ' '.
  wa_afield-do_sum    = ''.
  wa_afield-outputlen = 18.
  APPEND wa_afield TO it_fieldcat.


*VAlor dolar
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos   = i.
  wa_afield-fieldname = 'DMBE2'.
  wa_afield-scrtext_m = TEXT-a15.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key       = ' '.
  wa_afield-do_sum    = ''.
  wa_afield-outputlen = 18.
  APPEND wa_afield TO it_fieldcat.



*VALOR ATUALIZADO.
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_ATUALIZADO'.
  wa_afield-scrtext_m = TEXT-a18.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-outputlen     = 18.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.


*ACUM. MÊS ANT.
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_ACUM_MES_ANT'.
  wa_afield-scrtext_m = TEXT-a19.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-outputlen     = 18.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.

*ACUM. MÊS ATUAL
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_ACUM_MES_ATU'.
  wa_afield-scrtext_m = TEXT-a20.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-outputlen     = 18.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.

*VLR. VARIAÇÃO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_VARIACAO'.
  wa_afield-scrtext_m = TEXT-a21.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-outputlen     = 18.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.

*RESULTADO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RESULTADO_C'.
  wa_afield-scrtext_m = TEXT-a22.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-outputlen     = 12.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.

*DT.LCTO.VAR.
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_LCTO'.
  wa_afield-scrtext_m = TEXT-a23.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*DOC.VAR.
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DOC_LCTO'.
  wa_afield-scrtext_m = TEXT-a24.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-ref_field = 'BELNR'.
  wa_afield-ref_table   = 'BSEG'.
  wa_afield-key           = ' '.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

*FLAG ESTORNO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ESTORNO'.
  wa_afield-scrtext_m = TEXT-a25.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 08.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGDT'.
  wa_afield-scrtext_m = TEXT-a29.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'AUGBL'.
  wa_afield-scrtext_m = TEXT-a30.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_REV'.
  wa_afield-scrtext_m = TEXT-a46.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DOC_REV'.
  wa_afield-scrtext_m = TEXT-a47.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*USUÁRIO
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'USNAM'.
  wa_afield-scrtext_m = TEXT-a26.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  APPEND wa_afield TO it_fieldcat.

*DATA
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CPUDT'.
  wa_afield-scrtext_m = TEXT-a27.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*HORA
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CPUTM'.
  wa_afield-scrtext_m = TEXT-a28.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-key           = ' '.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_TRANSF
*&---------------------------------------------------------------------*
*       Configura informações
*----------------------------------------------------------------------*
FORM f_alv_transf .
  DATA: w_augdt TYPE datum.

  DATA: tl_zgl012 LIKE t_zgl012_avm OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF tl_skat OCCURS 0,
          saknr TYPE saknr,
          txt50 TYPE txt50_skat,
        END OF tl_skat.

  DATA: BEGIN OF tl_kna1 OCCURS 0,
          kunnr TYPE kunnr,
          name1 TYPE name1_gp,
          vbund TYPE kna1-vbund,
        END OF tl_kna1.

  DATA: BEGIN OF tl_lfa1 OCCURS 0,
          lifnr TYPE lifnr,
          name1 TYPE name1_gp,
          vbund TYPE lfa1-vbund,
        END OF tl_lfa1.

  CLEAR: wa_layout.

  wa_layout-zebra      = 'X'.
  wa_layout-no_rowmove = 'X'.
  wa_layout-no_rowins  = 'X'.
  wa_layout-no_rowmark = space.
  CLEAR wa_layout-grid_title .

  IF p_e_lanc = 'X'. "Se for reversão
    wa_layout-grid_title = TEXT-c03. "'Estorno de lançamentos'.
  ELSEIF p_c_lanc = 'X'.
    wa_layout-grid_title = TEXT-c02. "'Criação de lançamentos'.
  ELSEIF p_v_lanc = 'X'.
    wa_layout-grid_title = TEXT-c01. "'Visualização de lançamentos'.
  ENDIF.

  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt = ' '.
  wa_layout-box_fname  = 'MARK'.

  DO 3 TIMES.
    tl_zgl012[] = t_zgl012_avm[].

    CASE sy-index.
      WHEN 1.
        DELETE tl_zgl012 WHERE hkont = space.

        IF NOT tl_zgl012[] IS INITIAL.
          SELECT saknr txt50 FROM skat
            INTO TABLE tl_skat
             FOR ALL ENTRIES IN tl_zgl012
           WHERE spras = sy-langu
             AND saknr = tl_zgl012-hkont.

          IF sy-subrc <> 0.
            MESSAGE s398(00) WITH TEXT-021 space space space. "Erro ao obter descrição de contas
          ELSE.
            SORT tl_skat BY saknr.
          ENDIF.

        ENDIF.

      WHEN 2.
        DELETE tl_zgl012 WHERE kunnr = space.

        IF NOT tl_zgl012[] IS INITIAL.
          SELECT kunnr name1 vbund FROM kna1
            INTO TABLE tl_kna1
             FOR ALL ENTRIES IN tl_zgl012
           WHERE kunnr = tl_zgl012-kunnr.

          IF sy-subrc <> 0.
            MESSAGE s398(00) WITH TEXT-022 space space space. "Erro ao obter descrição de clientes
          ELSE.
            SORT tl_kna1 BY kunnr.
          ENDIF.

        ENDIF.

      WHEN 3.
        DELETE tl_zgl012 WHERE lifnr = space.

        IF NOT tl_zgl012[] IS INITIAL.
          SELECT lifnr name1 vbund
            FROM lfa1
            INTO TABLE tl_lfa1
             FOR ALL ENTRIES IN tl_zgl012
           WHERE lifnr = tl_zgl012-lifnr.

          IF sy-subrc <> 0.
            MESSAGE s398(00) WITH TEXT-023 space space space."Erro ao obter descrição de fornecedores
          ELSE.
            SORT tl_lfa1 BY lifnr.
          ENDIF.

        ENDIF.

    ENDCASE.

  ENDDO.

  LOOP AT t_zgl012_avm INTO wa_zgl012_aux.

    IF p_bukrs EQ '0101'.
      wa_zgl012_aux-vlr_atualizado   = trunc( wa_zgl012_aux-vlr_atualizado ).
      wa_zgl012_aux-vlr_acum_mes_atu = trunc( wa_zgl012_aux-vlr_acum_mes_atu ).
      wa_zgl012_aux-vlr_acum_mes_ant = trunc( wa_zgl012_aux-vlr_acum_mes_ant ).
      wa_zgl012_aux-vlr_variacao     = trunc( wa_zgl012_aux-vlr_variacao ).
    ENDIF.

    MOVE-CORRESPONDING wa_zgl012_aux TO o_alv.

    IF wa_zgl012_aux-moeda_atu = ''.
      o_alv-moeda_atu =  TEXT-a39.
    ELSE.
      o_alv-moeda_atu =  TEXT-a40.
    ENDIF.

    IF o_alv-vlr_variacao < 0.
      o_alv-resultado_c = TEXT-a41.
    ELSE.
      o_alv-resultado_c = TEXT-a42.
    ENDIF.

    IF ( NOT wa_zgl012_aux-hkont IS INITIAL ) AND
       ( wa_zgl012_aux-kunnr IS INITIAL )     AND
       ( wa_zgl012_aux-lifnr IS INITIAL ).
      o_alv-tipo =  TEXT-a43.
      o_alv-codigo = wa_zgl012_aux-hkont .
      READ TABLE tl_skat WITH KEY saknr = wa_zgl012_aux-hkont BINARY SEARCH.
      IF sy-subrc = 0.
        o_alv-descricao = tl_skat-txt50.
      ENDIF.
    ENDIF.

    IF NOT wa_zgl012_aux-kunnr IS INITIAL.
      o_alv-tipo = TEXT-a44.
      o_alv-codigo = wa_zgl012_aux-kunnr.
      READ TABLE tl_kna1 WITH KEY kunnr = wa_zgl012_aux-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        o_alv-descricao = tl_kna1-name1.
      ENDIF.

    ENDIF.

    IF NOT wa_zgl012_aux-lifnr IS INITIAL.
      o_alv-tipo = TEXT-a45.
      o_alv-codigo = wa_zgl012_aux-lifnr.
      READ TABLE tl_lfa1 WITH KEY lifnr = wa_zgl012_aux-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        o_alv-descricao = tl_lfa1-name1.
      ENDIF.
    ENDIF.

    o_alv-status = icon_led_red.

    "verificar aqui status Fb08

    IF wa_zgl012_aux-st_rev = 'S'.
      REFRESH t_zgl012_aux.
      SELECT * INTO TABLE t_zgl012_aux FROM zgl012_avm
       WHERE
         bukrs      EQ  wa_zgl012_aux-bukrs  AND
         dt_aval    LT  p_budat              AND
         belnr      EQ  wa_zgl012_aux-belnr  AND
         buzei      EQ  wa_zgl012_aux-buzei  AND
         moeda_atu  EQ  wa_zgl012_aux-moeda_atu AND
         estorno    NE 'X'           AND
         doc_rev    EQ ''. "
      IF t_zgl012_aux[] IS NOT INITIAL. "não reverteu
        o_alv-status = icon_red_light.
      ELSE.
        o_alv-status = icon_checked. "ICON_LED_GREEN.
      ENDIF.

      o_alv-vlr_variacao = o_alv-vlr_acum_mes_ant.
      IF  o_alv-vlr_variacao = 0.
        o_alv-status = icon_checked. "ICON_LED_GREEN.
      ENDIF.
      "Reversão nova
    ELSEIF  NOT wa_zgl012_aux-doc_lcto IS INITIAL OR o_alv-vlr_variacao = 0.
      o_alv-status = icon_led_green.
    ELSEIF wa_zgl012_aux-obj_key IS NOT INITIAL.
      READ TABLE it_zib_contabil_err INTO wa_zib_contabil_err WITH KEY obj_key = wa_zgl012_aux-obj_key BINARY SEARCH.
      IF sy-subrc = 0.
        o_alv-status = icon_incomplete.
      ENDIF.
    ENDIF.

    o_alv-dele   = wa_zgl012_aux-dele.

    APPEND o_alv.

    CLEAR o_alv.

  ENDLOOP.


  DELETE o_alv WHERE budat GT p_budat.

  IF p_e_lanc = 'X'. "Somente reversão
    DELETE o_alv WHERE status  NE icon_red_light AND status NE icon_checked. "Reversão
  ELSE.
    DELETE o_alv WHERE status  EQ icon_red_light OR status EQ icon_checked. "Reversão
  ENDIF.


ENDFORM.                    " F_ALV_TRANSF

*&---------------------------------------------------------------------*
*&      Form  F_ALV_IMPRIME
*&---------------------------------------------------------------------*
*       Apresenta o Relatório
*----------------------------------------------------------------------*
FORM f_alv_imprime .
  CALL SCREEN 9001.

ENDFORM.                    " F_ALV_IMPRIME

*&---------------------------------------------------------------------*
*&      Form  F_PF_STATUS_NOVO
*&---------------------------------------------------------------------*
*     Força um PF Status com o botão para salvar layout
*----------------------------------------------------------------------*
FORM f_pf_status_novo ."USING RT_EXTAB TYPE SLIS_T_EXTAB.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  IF p_e_lanc = 'X' . "reversão
    APPEND 'ESTORNO' TO fcode. "elimina o botao estorno do menu
  ENDIF.
  IF  p_v_lanc = 'X'.
    APPEND 'ESTORNO' TO fcode.
    APPEND 'LANCAR' TO fcode.
    APPEND '&REFRESH' TO fcode.
  ENDIF.
  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.
  SET TITLEBAR  'ZFTITLE'.

ENDFORM.                    " F_PF_STATUS_NOVO

*&---------------------------------------------------------------------*
*&      Form  F_AT_USER_COMMAND
*&---------------------------------------------------------------------*
*     Tratamento de comandos do usuário - Salvar e Duplo clique
*----------------------------------------------------------------------*
FORM f_at_user_command USING ucomm LIKE sy-ucomm
                             selfield TYPE kkblo_selfield.

  st_selfield = selfield.

  selfield-refresh = 'X'.

  CASE sy-ucomm.
    WHEN 'UP' OR 'BACK' .
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_AT_USER_COMMAND
*---------------------------------------------------------------------*
*      Form  f_top_of_page
*---------------------------------------------------------------------*
FORM f_top_of_page .
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header.

ENDFORM.                    " f_top_of_page


*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_valida_campos CHANGING c_erro.
  DATA: wl_date       TYPE datum,
        wl_data_c(10).


*valida data da avaliação. Tem que ser o último dia do mes
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = p_budat
    IMPORTING
      last_day_of_month = wl_date
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH TEXT-017 space space space. "Erro ao determinar dia!
  ELSE.
    IF wl_date <> p_budat.
      WRITE wl_date TO wl_data_c.
      MESSAGE i398(00) WITH TEXT-024 " Favor entrar último dia do
                            TEXT-034 " mês como parâmetro. No caso:
                            wl_data_c
                            space.

*      ADD 1 TO C_ERRO.
    ENDIF.
  ENDIF.

*pelo menos um dos checkbox esteja marcado:
  IF p_hkont = space AND p_kunnr = space AND p_lifnr = space AND p_hkont2 = space.
    MESSAGE i398(00) WITH TEXT-025 "Para partidas em aberto pelo menos
                          TEXT-035 "um dos checkbox deve ser marcado.
                          TEXT-036 " (Razão, Fornecedor ou Cliente)
                          space.
    ADD 1 TO c_erro.

  ENDIF.

*Valida período de acordo com a data da avaliação
  IF p_spmon <> p_budat(6).
    MESSAGE i398(00) WITH TEXT-026 "Período difere da data de avaliação
                          space
                          space
                          space.
    ADD 1 TO c_erro.

  ENDIF.

*valida data do documento e data do lançamento iguais a data de avaliacao
  IF p_budat <> p_budat2 OR p_budat <> p_augdt.
    MESSAGE i398(00) WITH TEXT-027 "Data do documento e data do lançamento
                          TEXT-037 "devem ser iguais a data fixada da avaliação
                          space.
    ADD 1 TO c_erro.

  ENDIF.

*  CALL FUNCTION 'Z_CONTROLE_FECHAMES'
*   EXPORTING
*     I_BUKRS    = P_BUKRS
*     I_DATA     = P_BUDAT
**      I_DEP_RESP = VG_DEPTO
*   IMPORTING
*     E_STATUS   = E_STATUS
*     E_MESSA    = E_MESSA
*   EXCEPTIONS
*     ERROR      = 1
*     OTHERS     = 2.
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*  IF  E_STATUS = 'E'.
*    MESSAGE E398(00) WITH E_MESSA.
*  ENDIF.

ENDFORM.                    "ZF_VALIDA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  ZF_LANCAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV      text
*----------------------------------------------------------------------*
FORM zf_lancar CHANGING p_alv LIKE o_alv
                        p_erro.

  DATA: v_stblg     TYPE bkpf-stblg,
        v_dat       TYPE zib_contabil-budat,
        v_moeda_atu TYPE zgl012_avm-moeda_atu.
  "ALRS
  CLEAR vobj_key.
  IF p_c_lanc = 'X'.
    IF wg_acao = 'ESTORNO'.
      CLEAR: v_stblg, p_erro.
      vobj_key = p_alv-obj_key.

      SUBMIT z_fb08_zgl042 WITH p_obj = p_alv-obj_key
      AND RETURN.


      SELECT SINGLE stblg
        FROM zib_contabil_chv
        INNER JOIN bkpf
        ON  bkpf~bukrs = zib_contabil_chv~bukrs
        AND bkpf~belnr = zib_contabil_chv~belnr
        AND bkpf~gjahr = zib_contabil_chv~gjahr
        INTO v_stblg
        WHERE zib_contabil_chv~obj_key = p_alv-obj_key.

      IF v_stblg IS INITIAL. "Não estornou
        p_erro = 'X'.
      ENDIF.
    ELSE.
      IF p_alv-lifnr <> space.
        IF p_alv-umskz = space.
          PERFORM grava_zib USING '1' CHANGING p_alv p_erro.
        ELSE.
          PERFORM grava_zib USING '2' CHANGING p_alv p_erro.
        ENDIF.
      ENDIF.

      IF p_alv-kunnr <> space.
        IF p_alv-umskz = space.
          PERFORM grava_zib USING '3' CHANGING p_alv p_erro.
        ELSE.
          PERFORM grava_zib  USING '4' CHANGING p_alv p_erro.
        ENDIF.
      ENDIF.

      IF p_alv-kunnr IS INITIAL AND p_alv-lifnr IS INITIAL AND
         p_alv-hkont <> space.
        PERFORM grava_zib USING '5' CHANGING  p_alv p_erro .
      ENDIF.
    ENDIF.
  ELSEIF p_e_lanc = 'X'."Reversão
    IF o_alv-moeda_atu+0(2) = '01'.
      CLEAR v_moeda_atu.
    ELSE.
      v_moeda_atu = c_usd.
    ENDIF.
    SELECT * INTO TABLE t_zgl012_aux FROM zgl012_avm
         WHERE
           bukrs      EQ  p_alv-bukrs  AND
           dt_aval    LT  p_budat      AND
           belnr      EQ  p_alv-belnr  AND
           buzei      EQ  p_alv-buzei  AND
           moeda_atu  EQ  v_moeda_atu  AND
           estorno    NE 'X'           AND
           doc_rev    EQ  ''
    ORDER BY dt_aval.

    CLEAR: v_stblg, p_erro.
    LOOP AT t_zgl012_aux .
      CONCATENATE p_alv-augdt+6(2) '.' p_alv-augdt+4(2) '.' p_alv-augdt+0(4) INTO v_dat.
      SUBMIT z_fb08_zgl042 WITH p_obj = t_zgl012_aux-obj_key
                           WITH p_dat = v_dat
                           WITH p_st  = '04'
      AND RETURN.

      WAIT UP TO 1 SECONDS.

      SELECT SINGLE stblg
        FROM zib_contabil_chv
        INNER JOIN bkpf
        ON  bkpf~bukrs = zib_contabil_chv~bukrs
        AND bkpf~belnr = zib_contabil_chv~belnr
        AND bkpf~gjahr = zib_contabil_chv~gjahr
        INTO v_stblg
        WHERE zib_contabil_chv~obj_key = t_zgl012_aux-obj_key.

      IF v_stblg IS INITIAL. "Não estornou
        p_erro = 'X'.
      ELSE.
        UPDATE zgl012_avm SET doc_rev = v_stblg
                              dt_rev  = p_alv-augdt
        WHERE  bukrs  EQ  t_zgl012_aux-bukrs    AND
           dt_aval    EQ  t_zgl012_aux-dt_aval  AND
           belnr      EQ  t_zgl012_aux-belnr    AND
           buzei      EQ  t_zgl012_aux-buzei    AND
           moeda_atu  EQ  t_zgl012_aux-moeda_atu.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "ZF_LANCAR



*&---------------------------------------------------------------------*
*&      Module  status_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE status_9001 OUTPUT.
  DATA: w_dt    TYPE sy-datum,
        tabix   TYPE sy-tabix,
        w_dtzib TYPE zgl012_avm-dt_aval.

  PERFORM f_pf_status_novo .

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.

  ENDIF.

  IF NOT cl_grid IS INITIAL.
    PERFORM zf_alv_header USING '2'.
    CALL METHOD cl_grid->refresh_table_display.

    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    PERFORM zf_alv_header USING '1'.

    IF editcontainer IS INITIAL .
      CREATE OBJECT editcontainer
        EXPORTING
          container_name = 'HEADER'.

    ENDIF .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.

    CREATE OBJECT cl_grid
      EXPORTING
        i_parent      = cl_container_95
        i_appl_events = 'X'.

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*Limpa os duplicados comparando BELNR e BSCHL caso iguais manter o registro que possuir AUGBL preenchido
    SORT o_alv BY belnr buzei bschl dele ASCENDING augbl dt_lcto doc_lcto DESCENDING.

    wg_x_variant-report = sy-repid.
    wg_save             = 'X'.
    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = wg_x_variant
        i_save          = wg_save        "IS_VARIANT = WG_VARIANT
        is_layout       = wa_layout
      CHANGING
        it_fieldcatalog = it_fieldcat[]
*       IT_SORT         = I_SORT[]
        it_outtab       = o_alv[].

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->catch_hotspot      FOR cl_grid.
    SET HANDLER event_receiver->handle_top_of_page FOR cl_grid.

  ENDIF.

ENDMODULE.                 " status_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  DATA: v_valid(1) TYPE c,
        indrow     TYPE lvc_t_row,
        w_ind      TYPE lvc_t_row WITH HEADER LINE,
        v_mod(1)   TYPE c,
        wl_tabix   TYPE sy-tabix,
        wl_erro(1),
        "E_STATUS(1),
        "E_MESSA(64),
        wl_cont    TYPE i.


  IF NOT cl_grid IS INITIAL.
    CALL METHOD cl_grid->dispatch
      EXPORTING
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    IF sy-ucomm IS INITIAL.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = is_stable.
    ENDIF.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH o_alv.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '&REFRESH'.
      PERFORM atualiza_tela.
    WHEN 'LANCAR' OR 'ESTORNO'.
      wg_acao = sy-ucomm.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = p_bukrs
          i_data   = p_budat
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF  e_status = 'E'.
        MESSAGE e398(00) WITH e_messa.
      ENDIF.

      CHECK p_v_lanc IS INITIAL.
      IF t_t030h[] IS INITIAL.
        SELECT ktopl hkont waers curtp lsbew lhbew FROM t030h
          INTO TABLE t_t030h
           FOR ALL ENTRIES IN o_alv
         WHERE ktopl = c_0050
           AND hkont = o_alv-hkont.
      ENDIF.

      IF t_0025[] IS INITIAL
      AND t_t030h[] IS NOT INITIAL.
        SELECT *
          FROM zfit0025
          INTO TABLE t_0025
           FOR ALL ENTRIES IN t_t030h
         WHERE saknr_p EQ t_t030h-lsbew.

        SELECT *
          FROM zfit0025
     APPENDING TABLE t_0025
           FOR ALL ENTRIES IN t_t030h
         WHERE saknr_p EQ t_t030h-lhbew.

        SORT: t_0025 BY saknr_p.

      ENDIF.

      REFRESH indrow.

      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = indrow.

      IF NOT indrow IS INITIAL.
        DELETE indrow WHERE NOT rowtype IS INITIAL.
      ENDIF.


      LOOP AT o_alv.
        o_alv-mark = ' '.
        MODIFY o_alv.
      ENDLOOP.

      LOOP AT indrow INTO w_ind.
        READ TABLE o_alv INDEX w_ind-index.
        o_alv-mark = 'X'.
        MODIFY o_alv INDEX w_ind-index.

      ENDLOOP.

      READ TABLE o_alv WITH KEY mark   = 'X'
                                  st_rev = 'X'.

      IF p_e_lanc = 'X' OR sy-subrc = 0 OR wg_acao = 'ESTORNO'.
        CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
          EXPORTING
            tcode  = 'FB08'
          EXCEPTIONS
            ok     = 0
            not_ok = 2
            OTHERS = 3.
        IF sy-subrc <> 0.
          MESSAGE TEXT-e01 TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.

      CLEAR: wa_zgl012_avm, wl_cont.
      READ TABLE o_alv WITH KEY mark   = 'X'
                                status = icon_okay.
      IF sy-subrc = 0.
        MESSAGE w398(00) WITH TEXT-028 "Existe registro com valor de variação
                              TEXT-038 "igual a zero. Reveja a seleção linha:
                              sy-tabix.
      ELSE.

        LOOP AT o_alv WHERE mark = 'X'.
          CHECK ( o_alv-status = icon_led_green AND wg_acao = 'ESTORNO' ) OR
                ( ( o_alv-status = icon_led_red OR o_alv-status = icon_red_light OR o_alv-status = icon_incomplete ) AND
                wg_acao = 'LANCAR' ).

          IF wg_acao = 'LANCAR'.

            IF o_alv-moeda_atu+0(2) = '01'.
              wa_zgl012_avm-moeda_atu = ''.
            ELSE.
              wa_zgl012_avm-moeda_atu =  c_usd.
            ENDIF.

            IF o_alv-status NE icon_incomplete.
              SELECT SINGLE *
                FROM  zgl012_avm
                INTO  wa_zgl012_avm
                WHERE bukrs     EQ o_alv-bukrs
                AND   dt_aval   EQ o_alv-dt_aval
                AND   belnr     EQ o_alv-belnr
                AND   buzei     EQ o_alv-buzei
                AND   moeda_atu EQ wa_zgl012_avm-moeda_atu
                AND   obj_key   NE ''.

              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.

          ENDIF.
          IF o_alv-status = icon_led_green AND wg_acao = 'ESTORNO' AND o_alv-vlr_variacao = 0.
            CONTINUE.
          ENDIF.

          wl_tabix = sy-tabix.

          CLEAR wl_st_rev.

          IF o_alv-status = icon_red_light. "Reversão
            wl_st_rev = 'X'.
          ENDIF.

          CLEAR wl_erro.
          PERFORM zf_lancar CHANGING o_alv wl_erro.

          IF wl_st_rev = 'X'.
            IF wl_erro IS INITIAL.
              o_alv-status   = icon_led_green.
            ELSE.
              o_alv-status   = icon_incomplete.
            ENDIF.
            MODIFY o_alv INDEX wl_tabix TRANSPORTING status.
            COMMIT WORK AND WAIT .
            wl_cont = wl_cont + 1.
            CONTINUE.
          ENDIF.

          IF wl_erro IS INITIAL.
            IF wg_acao = 'ESTORNO'.
              CLEAR: o_alv-dt_lcto, o_alv-doc_lcto.
              o_alv-estorno = 'X'.
              o_alv-status = icon_led_red.
              wl_cont = wl_cont + 1.
              DELETE FROM zgl012_avm WHERE obj_key = o_alv-obj_key.
              COMMIT WORK.
              CLEAR vobj_key.
            ELSE.
              CLEAR: o_alv-estorno.
              o_alv-status   = icon_led_green.
              CLEAR o_alv-doc_lcto.
            ENDIF.

            o_alv-status = icon_generate .
            o_alv-obj_key = vobj_key.

            MOVE: sy-uname TO o_alv-usnam,
                  sy-uzeit TO o_alv-cputm,
                  sy-datum TO o_alv-cpudt.
            MODIFY o_alv INDEX wl_tabix TRANSPORTING dt_lcto doc_lcto status estorno obj_key st_rev usnam cputm cpudt.

            MOVE-CORRESPONDING o_alv TO wa_zgl012_avm .

            IF o_alv-moeda_atu+0(2) = '01'.
              wa_zgl012_avm-moeda_atu = ''.
            ELSE.
              wa_zgl012_avm-moeda_atu =  c_usd.
            ENDIF.

            MOVE: sy-uname TO wa_zgl012_avm-usnam,
                  sy-uzeit TO wa_zgl012_avm-cputm,
                  sy-datum TO wa_zgl012_avm-cpudt,
                  vobj_key TO wa_zgl012_avm-obj_key.

            IF wl_st_rev = 'X'.
              wa_zgl012_avm-st_rev = 'X'. "Marcar como reversão OK
            ENDIF.
            IF wg_acao NE 'ESTORNO'.
              MODIFY zgl012_avm FROM wa_zgl012_avm.
              IF sy-subrc = 0.
                COMMIT WORK AND WAIT .
                wl_cont = wl_cont + 1.
              ENDIF.
            ENDIF.
          ELSE.
            o_alv-status = icon_led_red.
            MODIFY o_alv INDEX wl_tabix TRANSPORTING status.
          ENDIF.

          CLEAR wa_zgl012_avm.

        ENDLOOP.

        IF wl_cont > 0.
          MESSAGE i398(00) WITH wl_cont TEXT-029  "registro(s) executado(s) com sucesso.
                                        TEXT-041. " (gravados na ZTGL012_AVM).
        ELSE.
          MESSAGE i398(00) WITH TEXT-030. "'Nenhum registro executado com sucesso.'
        ENDIF.

      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " user_command_9001  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_INICIALIZA_VARIANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_inicializa_variante.
  CLEAR wg_variant.

  wg_save = 'X'.
  wg_variant-report = wg_repname.
  wg_x_variant = wg_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = wg_save
    CHANGING
      cs_variant = wg_x_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_vari = wg_x_variant-variant.
  ENDIF.

ENDFORM. " ZF_inicializa_variante
*&---------------------------------------------------------------------*
*&      Form  ZF_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_at_selection_screen .
* ALV Layout variant
  IF NOT p_vari IS INITIAL.
    MOVE wg_variant TO wg_x_variant.
    MOVE p_vari TO wg_x_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = wg_save
      CHANGING
        cs_variant = wg_x_variant.

    wg_variant = wg_x_variant.

  ELSE.
    PERFORM zf_inicializa_variante.
  ENDIF.

ENDFORM.                    "zf_at_selection_screen

*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       Free Objects
*----------------------------------------------------------------------*
FORM free_objects .
  CALL METHOD cl_grid->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_container_95->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_container_05->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD editcontainer->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " free_objects



*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IM_OBJ_DYNDOC_ID  text
*      -->IM_TABLE_INDEX    text
*----------------------------------------------------------------------*
FORM event_top_of_page USING im_obj_dyndoc_id TYPE REF TO cl_dd_document
                             im_table_index   TYPE syindex .

  PERFORM zf_alv_header  USING '2'.

ENDFORM.                    "EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_zib USING    p_tipo
               CHANGING p_alv LIKE o_alv
                        p_erro.

  DATA: wl_data(10),
        wl_descricao(1),
        wl_data_aval(10),
        wl_bktxt(7),
        wl_newbs(2),
        wl_newbs2(2),
        wl_newko2(10),
        wl_vlr_variacao  TYPE zgl012_avm-vlr_variacao,

        wl_divisao(4),
        wl_bupla(4),
        wl_bewar         TYPE zfit0030-bewar,
        xprov_dolar      TYPE i.

  DATA: BEGIN OF tl_aux OCCURS 0,
          belnr   TYPE belnr_d,
          dt_aval TYPE budat,
        END OF tl_aux.

  IF p_alv-gsber IS INITIAL.
    IF p_alv-bukrs = '0200' .
      wl_divisao = 'S201'.
    ELSEIF p_alv-bukrs = '0201' .
      wl_divisao = 'H201'.
    ELSEIF p_alv-bukrs = '0202' .
      wl_divisao = 'H202'.
    ELSEIF p_alv-bukrs = '0101' .
      wl_divisao = 'F101'.
    ELSE.
      CONCATENATE p_alv-bukrs+2(2) '01'
      INTO wl_divisao.
    ENDIF.
  ELSE.
    MOVE p_alv-gsber TO wl_divisao.
  ENDIF.

  wl_bupla = wl_divisao.
  IF p_alv-bukrs = '0004'.
    wl_bupla = '0401'.
  ENDIF.


  IF p_alv-bukrs = '0100'.
    CLEAR: wl_bupla, wl_divisao.
  ENDIF.

  WRITE p_alv-dt_aval TO wl_data.

  READ TABLE t_t030h INTO wa_t030h WITH KEY hkont = p_alv-hkont.
  CLEAR wl_descricao.

  IF p_alv-vlr_variacao GE 0.
    wl_vlr_variacao = p_alv-vlr_variacao.
  ELSE.
    wl_vlr_variacao = p_alv-vlr_variacao * -1.
  ENDIF.

  SELECT SINGLE bewar
    FROM zfit0030
    INTO wl_bewar
   WHERE hkont  = p_alv-hkont
     AND cond  IN ('C','CV').

  IF sy-subrc NE 0.
    SELECT SINGLE bewar
      FROM zfit0030
      INTO wl_bewar
     WHERE hkont = p_alv-hkont.

  ENDIF.
  "alrs
  CASE p_tipo.
    WHEN '1'.
      IF ( p_alv-vlr_variacao > 0 AND ( p_alv-bschl = '31' OR
                                        p_alv-bschl = '34' ) )
      OR (  p_alv-vlr_variacao > 0 AND ( ( p_alv-bschl GE '21'
                                           AND p_alv-bschl LE '39' ) AND
                                         ( p_alv-bschl NE '23' AND
                                           p_alv-bschl NE '30' AND
                                           p_alv-bschl NE '33' ) ) ).
        wl_newbs  = '31'.
        wl_newbs2 = '40'.
        wl_newko2 = wa_t030h-lsbew.

      ENDIF.
      IF ( p_alv-vlr_variacao < 0 AND ( p_alv-bschl = '31' OR
                                        p_alv-bschl = '34' ) )
      OR ( p_alv-vlr_variacao < 0 AND ( ( p_alv-bschl GE '21'
                                          AND p_alv-bschl LE '39' ) AND
                                        ( p_alv-bschl NE '23' AND
                                          p_alv-bschl NE '30' AND
                                          p_alv-bschl NE '33' ) ) ).
        wl_newbs = '21'.
        wl_newbs2 = '50'.
        wl_newko2 = wa_t030h-lhbew.

      ENDIF.
      IF p_alv-status = icon_red_light.
        IF p_alv-vlr_variacao > 0 AND p_alv-umskz = space.
          wl_newbs = '31'.                                "'21'.
          wl_newbs2 = '40'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao > 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0 AND p_alv-umskz = space.
          wl_newbs  = '21'.                               "'31'.
          wl_newbs2 = '50'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN '2'.
      IF ( p_alv-vlr_variacao < 0 AND p_alv-bschl = '39' )
      OR ( p_alv-vlr_variacao < 0 AND p_alv-bschl = '29' ).
        wl_newbs  = '29'.
        wl_newbs2 = '50'.
        wl_newko2 = wa_t030h-lhbew.

      ENDIF.
      IF ( p_alv-vlr_variacao > 0 AND p_alv-bschl = '39' )
      OR ( p_alv-vlr_variacao > 0 AND p_alv-bschl = '29' ).
        wl_newbs = '39'.
        wl_newbs2 = '40'.
        wl_newko2 = wa_t030h-lsbew.

      ENDIF.
      IF p_alv-status = icon_red_light.
        IF p_alv-vlr_variacao > 0 AND p_alv-umskz <> space.
          wl_newbs = '39'.                                "'29'.
          wl_newbs2 = '40'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao > 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0 AND p_alv-umskz <> space..
          wl_newbs  = '29'.                               "'39'.
          wl_newbs2 = '50'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '3'.
      IF ( p_alv-vlr_variacao < 0 AND (  p_alv-bschl = '11' OR
                                         p_alv-bschl = '17' ) )
      OR ( p_alv-vlr_variacao < 0 AND ( ( p_alv-bschl GE '01' AND
                                          p_alv-bschl LE '19' ) AND
                                          p_alv-bschl NE '10' ) ).
        wl_newbs  = '01'.
        wl_newbs2 = '50'.
        wl_newko2 = wa_t030h-lhbew.

      ENDIF.
      IF ( p_alv-vlr_variacao > 0 AND ( p_alv-bschl = '11' OR
                                        p_alv-bschl = '17' ) )
      OR ( p_alv-vlr_variacao > 0 AND ( ( p_alv-bschl GE '01' AND
                                          p_alv-bschl LE '19' ) AND
                                          p_alv-bschl NE '10' ) ).
        wl_newbs = '11'.
        wl_newbs2 = '40'.
        wl_newko2 = wa_t030h-lsbew.

      ENDIF.
      IF p_alv-status = icon_red_light.
        IF p_alv-vlr_variacao > 0 AND p_alv-umskz = space.
          wl_newbs = '11'.                                "'01'.
          wl_newbs2 = '40'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao > 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0 AND p_alv-umskz = space.
          wl_newbs  = '01'.                               "'11'.
          wl_newbs2 = '50'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '4'.

      IF ( p_alv-vlr_variacao < 0 AND p_alv-bschl = '19' )
         OR
         ( p_alv-vlr_variacao < 0 AND p_alv-bschl = '09' ).
        wl_newbs = '09'.
        wl_newbs2 = '50'.
        wl_newko2 = wa_t030h-lhbew.

      ENDIF.
      IF ( p_alv-vlr_variacao > 0 AND p_alv-bschl = '19' )
      OR ( p_alv-vlr_variacao > 0 AND p_alv-bschl = '09' ).
        wl_newbs  = '19'.
        wl_newbs2 = '40'.
        wl_newko2 = wa_t030h-lsbew.

      ENDIF.
      IF p_alv-status = icon_red_light.
        IF p_alv-vlr_variacao > 0 AND p_alv-umskz <> space.
          wl_newbs = '19'.                                "'09'.
          wl_newbs2 = '40'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao > 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0 AND p_alv-umskz <> space.
          wl_newbs  = '09'.                               "'19'.
          wl_newbs2 = '50'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '5'.
      IF ( p_alv-vlr_variacao > 0 AND ( p_alv-bschl = '40' OR
                                        p_alv-bschl = '86' ) )
      OR ( p_alv-vlr_variacao > 0 AND ( p_alv-bschl = '50' OR
                                        p_alv-bschl = '96' ) ).
        wl_newbs  = '50'.
        wl_newbs2 = '40'.
        wl_newko2 = wa_t030h-lsbew.

      ENDIF.

      IF ( p_alv-vlr_variacao < 0 AND ( p_alv-bschl = '50' OR
                                        p_alv-bschl = '96' ) )
      OR ( p_alv-vlr_variacao < 0 AND ( p_alv-bschl = '40' OR
                                        p_alv-bschl = '86' ) ).
        wl_newbs = '40'.
        wl_newbs2 = '50'.
        wl_newko2 = wa_t030h-lhbew.

      ENDIF.
      IF p_alv-status = icon_red_light.
        IF p_alv-vlr_variacao > 0 AND p_alv-umskz <> space.
          wl_newbs = '50'.                                "'40'.
          wl_newbs2 = '40'.                               "'50'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao > 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lsbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0 AND p_alv-umskz <> space.
          wl_newbs  = '40'.                               "'50'.
          wl_newbs2 = '50'.                               "'40'.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ELSEIF p_alv-vlr_variacao < 0.
**        Caso seja uma reversão
          CLEAR: wa_0025.
          READ TABLE t_0025 INTO wa_0025
            WITH KEY saknr_p = wa_t030h-lhbew
                       BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: wa_0025-saknr_r TO wl_newko2.
          ELSE.
            CLEAR: wl_newko2.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

  "alrs
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_GL'
    IMPORTING
      number      = vseq.

  vnum = vseq .

  vseqitem = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vnum
    IMPORTING
      output = vnum.

  SELECT SINGLE bukrs land1
    FROM t001
    INTO wa_t001
   WHERE bukrs = p_bukrs.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE land1 waers
    FROM t005
    INTO wa_t005
   WHERE land1 = wa_t001-land1.

  " Primeira Perna lançamento contábil
  CONCATENATE 'ZGL042' vnum  p_spmon+0(4) INTO wa_zib_contabil-obj_key.
  vobj_key = wa_zib_contabil-obj_key.
  vseqitem = 1.
  wa_zib_contabil-seqitem   = vseqitem.
  wa_zib_contabil-xblnr     = p_alv-resultado_c.
  wa_zib_contabil-bschl     = wl_newbs. " P_ALV-BSCHL.
  wa_zib_contabil-gsber     = wl_divisao.
  wa_zib_contabil-bukrs     = p_bukrs.
  wa_zib_contabil-interface = '35'.
  CONCATENATE p_spmon+4(2) p_spmon+0(4) INTO wa_zib_contabil-bktxt SEPARATED BY '.'.
  wa_zib_contabil-bldat     = wl_data.
  wa_zib_contabil-budat     = wl_data.
  wa_zib_contabil-gjahr     = p_spmon+0(4).
  wa_zib_contabil-monat     = p_spmon+4(2).
  wa_zib_contabil-blart     = 'VC'.

  CLEAR wa_zib_contabil-hkont2.

  IF o_alv-tipo = TEXT-a43. "Razao
    wa_zib_contabil-hkont     = p_alv-hkont.
    CLEAR wa_zib_contabil-hkont2.
  ELSE.
    wa_zib_contabil-hkont     = p_alv-codigo.
*    WA_ZIB_CONTABIL-HKONT2    = P_ALV-HKONT.
  ENDIF.

  wa_zib_contabil-umskz     = p_alv-umskz.
  wa_zib_contabil-wrbtr     = 0.
  wa_zib_contabil-waers     = p_alv-waers.
  wa_zib_contabil-bupla     = wl_bupla.
  wa_zib_contabil-zuonr     = p_alv-belnr.

  IF p_tipo = 'E'.
*    CONCATENATE 'Estorno VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
    CONCATENATE TEXT-a53 o_alv-descricao INTO wa_zib_contabil-sgtxt.
  ELSE.
    IF p_alv-status EQ '@0A@'.
*      CONCATENATE 'Reversão Provisão VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
      CONCATENATE TEXT-a54 o_alv-descricao INTO wa_zib_contabil-sgtxt.
    ELSE.
*      CONCATENATE 'Provisão VC-' O_ALV-DESCRICAO INTO WA_ZIB_CONTABIL-SGTXT.
      CONCATENATE TEXT-a55 o_alv-descricao INTO wa_zib_contabil-sgtxt.
    ENDIF.
  ENDIF.

  wa_zib_contabil-waers_i = 'X'.
  IF o_alv-moeda_atu+0(2) = '01'. "Interna
    wa_zib_contabil-dmbtr     = wl_vlr_variacao.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = 0.
  ELSE.
    wa_zib_contabil-dmbtr     = 0.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = wl_vlr_variacao.
  ENDIF.

  wa_zib_contabil-rg_atualizado  = 'N'.
  wa_zib_contabil-bewar     = wl_bewar.

  INSERT INTO  zib_contabil VALUES wa_zib_contabil.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    p_erro = 'X'.
    CLEAR vobj_key.
  ELSE.
    COMMIT WORK.
  ENDIF.

  CLEAR  wa_zib_contabil.

  " segunda perna lançamento contábil
  wa_zib_contabil-obj_key = vobj_key .
  vseqitem = 2.
  wa_zib_contabil-seqitem   = vseqitem.
  wa_zib_contabil-xblnr     = p_alv-resultado_c.
  wa_zib_contabil-bschl     = wl_newbs2.
  wa_zib_contabil-gsber     = wl_divisao.
  wa_zib_contabil-bukrs     = p_bukrs.
  wa_zib_contabil-interface = '35'.
  CONCATENATE p_spmon+4(2) p_spmon+0(4) INTO wa_zib_contabil-bktxt SEPARATED BY '.'.
  wa_zib_contabil-bldat     = wl_data.
  wa_zib_contabil-budat     = wl_data.
  wa_zib_contabil-gjahr     = p_spmon+0(4).
  wa_zib_contabil-monat     = p_spmon+4(2).
  wa_zib_contabil-blart     = 'VC'.
  wa_zib_contabil-hkont     = wl_newko2.
  wa_zib_contabil-umskz     = p_alv-umskz.
  wa_zib_contabil-wrbtr     = 0.
  wa_zib_contabil-waers     = p_alv-waers.
  wa_zib_contabil-bupla     = wl_bupla.
  wa_zib_contabil-zuonr     = p_alv-belnr.

  IF p_tipo = 'E'.
    CONCATENATE TEXT-a53 o_alv-descricao INTO wa_zib_contabil-sgtxt.
  ELSE.
    IF p_alv-status EQ '@0A@'.
      CONCATENATE TEXT-a54  o_alv-descricao INTO wa_zib_contabil-sgtxt.
    ELSE.
      CONCATENATE TEXT-a55  o_alv-descricao INTO wa_zib_contabil-sgtxt.
    ENDIF.
  ENDIF.
  wa_zib_contabil-waers_i   = 'X'.
  IF o_alv-moeda_atu+0(2) = '01'. "Interna
    wa_zib_contabil-dmbtr     = wl_vlr_variacao.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = 0.
  ELSE.
    wa_zib_contabil-dmbtr     = 0.
    wa_zib_contabil-waers_f   = 'USD'.
    wa_zib_contabil-dmbe2     = wl_vlr_variacao.
  ENDIF.
  wa_zib_contabil-rg_atualizado  = 'N'.
  wa_zib_contabil-bewar     = wl_bewar.

  INSERT INTO  zib_contabil VALUES wa_zib_contabil.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    p_erro = 'X'.
    CLEAR vobj_key.
  ELSE.
    COMMIT WORK.
  ENDIF.
  CLEAR  wa_zib_contabil.

ENDFORM.                    " GRAVA_ZIB
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_tela .
  DATA: flag_mod(1), tabix TYPE sy-tabix.

  LOOP AT o_alv.

    IF o_alv-obj_key IS INITIAL AND  o_alv-estorno NE 'X'.
      CONTINUE.
    ENDIF.

    tabix = sy-tabix.
    flag_mod = 'N'.

    IF o_alv-estorno = 'X'.
      CLEAR: o_alv-dt_lcto, o_alv-doc_lcto.
      o_alv-status = icon_led_red.
      flag_mod = 'S'.
      MODIFY o_alv INDEX tabix TRANSPORTING dt_lcto doc_lcto status.
      CONTINUE.
    ELSE.
      IF o_alv-vlr_variacao = 0.
        o_alv-status = icon_led_green.
      ENDIF.

      IF o_alv-doc_lcto IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      SELECT SINGLE  zib_contabil_chv~mandt
             zib_contabil_chv~obj_key
             zib_contabil_chv~belnr
             zib_contabil_chv~bukrs
             zib_contabil_chv~gjahr
             zib_contabil~bldat
        FROM zib_contabil_chv INNER JOIN zib_contabil
        ON zib_contabil~obj_key EQ zib_contabil_chv~obj_key
        INTO wa_zib_contabil_chv
        WHERE zib_contabil_chv~obj_key = o_alv-obj_key.

      IF sy-subrc = 0.
        CONCATENATE wa_zib_contabil_chv-bldat+6(4) wa_zib_contabil_chv-bldat+3(2) wa_zib_contabil_chv-bldat+0(2) INTO w_dtzib.
        IF w_dtzib+0(6) = o_alv-dt_aval+0(6).
          o_alv-status = icon_led_green.
          o_alv-doc_lcto = wa_zib_contabil_chv-belnr.
          o_alv-dt_lcto  = w_dtzib.
          flag_mod = 'S'.
        ENDIF.
        "FB08
        SELECT SINGLE   bukrs belnr  gjahr  budat stblg stjah
          FROM bkpf
          INTO wg_bkpf_fb08
          WHERE bukrs EQ wa_zib_contabil_chv-bukrs
          AND   belnr EQ wa_zib_contabil_chv-belnr
          AND   gjahr EQ wa_zib_contabil_chv-gjahr
          AND   stblg NE ''.
        IF sy-subrc = 0.
          SELECT SINGLE   bukrs belnr gjahr budat
             FROM bkpf
             INTO wg_bkpf_fb08_e
             WHERE bukrs EQ wg_bkpf_fb08-bukrs
             AND   belnr EQ wg_bkpf_fb08-stblg
             AND   gjahr EQ wg_bkpf_fb08-stjah.
          IF wg_bkpf_fb08_e-budat+0(6) = wg_bkpf_fb08-budat+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
            CLEAR: o_alv-dt_lcto, o_alv-doc_lcto, o_alv-obj_key.
            o_alv-status = icon_led_red. "Refazer variação
          ELSE.
            o_alv-status = icon_checked. "Reversão com sucesso
          ENDIF.
          flag_mod = 'S'.
        ENDIF.
      ELSEIF o_alv-obj_key IS NOT INITIAL.
        SELECT SINGLE *
         FROM zib_contabil_err
         INTO wa_zib_contabil_err
         WHERE obj_key = o_alv-obj_key.
        IF sy-subrc = 0.
          o_alv-status = icon_incomplete.
          flag_mod = 'S'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF flag_mod = 'S'.
      MODIFY o_alv INDEX tabix TRANSPORTING dt_lcto doc_lcto status.
      MOVE-CORRESPONDING o_alv TO wa_zgl012_avm .
      IF o_alv-moeda_atu+0(2) = '01'.
        CLEAR wa_zgl012_avm-moeda_atu.
      ELSE.
        wa_zgl012_avm-moeda_atu = 'USD'.
      ENDIF.

      IF wa_zgl012_avm-usnam IS INITIAL.
        MOVE: sy-uname TO wa_zgl012_avm-usnam,
              sy-uzeit TO wa_zgl012_avm-cputm,
              sy-datum TO wa_zgl012_avm-cpudt.
      ENDIF.

      MODIFY zgl012_avm FROM wa_zgl012_avm.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    CLEAR: wa_zgl012_avm.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_CONTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_buscar_conta .
  DATA: BEGIN OF tl_temp OCCURS 0,
          saknr TYPE ska1-saknr,
          txt50 TYPE skat-txt50,
        END OF tl_temp.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  SELECT a~saknr txt50
    INTO TABLE tl_temp
    FROM ska1 AS a
   INNER JOIN gl_acct_cc AS b ON b~saknr EQ a~saknr
   INNER JOIN skat AS c ON c~saknr EQ a~saknr AND c~ktopl EQ b~ktopl AND c~spras EQ sy-langu
   WHERE ktoks IN ('YB01','YB04')
     AND bukrs EQ p_bukrs.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SAKNR'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'SAKNR'
        value_org       = 'S'
      TABLES
        value_tab       = tl_temp
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

  ENDIF.

ENDFORM.                    " F_BUSCAR_CONTA
*&---------------------------------------------------------------------*
*&      Form  F_CONTA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_conta2 .
  DATA: tl_ska1             TYPE TABLE OF ska1 WITH HEADER LINE,
        tl_skat             TYPE TABLE OF skat WITH HEADER LINE,
        tl_skb1             TYPE TABLE OF skb1 WITH HEADER LINE,
        tl_zsaldo_cta_moeda TYPE TABLE OF zsaldo_cta_moeda WITH HEADER LINE,
        wa_skb1             TYPE skb1.

  DATA: it_contas         TYPE zct_emp_contas,
        wa_contas         TYPE zlc_emp_contas,

        it_contas2        TYPE zct_emp_contas,
        wa_contas2        TYPE zlc_emp_contas,

        tg_tcurr          TYPE TABLE OF tcurr,
        wg_tcurr          TYPE tcurr,
        it_bsxs           TYPE TABLE OF bsis WITH HEADER LINE,
        it_bsxs_g         TYPE TABLE OF bsis WITH HEADER LINE,
        it_saldo_contas   TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldo_contas_2 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
        it_saldo_contas_3 TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE.

  DATA: wl_saldo_mi        TYPE faglflext-hslvt,
        wl_saldo_mi2       TYPE faglflext-kslvt,
        wl_saldo_mi3       TYPE faglflext-oslvt,
        wl_saldo_doc       TYPE bsis-wrbtr,
        vg_last_day_aux(8),
        w_loop             TYPE i,
        vl_moeda_ok        TYPE c,
        w_moeda            TYPE zgl012_avm-waers.

  DATA: refe1	TYPE hslxx12,
        refe2	TYPE hslxx12,
        vmes  TYPE monat,
        tabix TYPE sy-tabix.

  CONCATENATE p_budat+0(6) '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  SELECT *
    FROM zsaldo_cta_moeda
    INTO TABLE tl_zsaldo_cta_moeda
    WHERE bukrs = p_bukrs.

  SORT tl_zsaldo_cta_moeda  BY saknr.

  RANGES: r_ktoks FOR ska1-ktoks.

  SELECT SINGLE *                      "#EC CI_DB_OPERATION_OK[2431747]
      FROM t882g
      INTO wg_t882g
       WHERE rbukrs = p_bukrs.

  CLEAR: r_ktoks.
  r_ktoks-sign   = 'I'.
  r_ktoks-option = 'EQ'.
  r_ktoks-low    = 'YB01'.
  r_ktoks-high   = 'YB01'.
  APPEND r_ktoks.

  r_ktoks-low    = 'YB02'.
  r_ktoks-high   = 'YB02'.
  APPEND r_ktoks.

  r_ktoks-low    = 'YB04'.
  r_ktoks-high   = 'YB04'.
  APPEND r_ktoks.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2389136]
      FROM ska1                        "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE tl_ska1
      WHERE ktopl	=	'0050'
      AND   ktoks	IN r_ktoks
      AND   saknr IN s_hkont2.

  IF tl_ska1[] IS INITIAL.
    MESSAGE i398(00) WITH TEXT-033."Não Encontrado contas do Razão p/ a Seleção
    EXIT.
  ENDIF.

  SELECT *                             "#EC CI_DB_OPERATION_OK[2431747]
     FROM skb1
     INTO TABLE tl_skb1
     FOR ALL ENTRIES IN tl_ska1
     WHERE bukrs  = p_bukrs
     AND   saknr  = tl_ska1-saknr
     AND   xopvw  = ''.

  IF tl_skb1[] IS INITIAL.
    MESSAGE i398(00) WITH TEXT-031 "Não Encontrado contas do Razão
                          TEXT-039 "p/ a Seleção (empresa)
                          TEXT-040. "administração por partidas em aberto
    EXIT.
  ENDIF.

  SELECT ktopl hkont waers curtp lsbew lhbew FROM t030h
      INTO TABLE t_t030h
      FOR ALL ENTRIES IN tl_skb1
      WHERE ktopl = c_0050 AND
            hkont = tl_skb1-saknr.

  SORT t_t030h BY hkont.
  LOOP AT tl_skb1 INTO wa_skb1.
    tabix = sy-tabix.
    READ TABLE t_t030h INTO wa_t030h WITH KEY hkont = wa_skb1-saknr BINARY SEARCH.
    IF sy-subrc NE 0.
      CLEAR wa_skb1-saknr.
      MODIFY tl_skb1 FROM wa_skb1 INDEX tabix TRANSPORTING saknr.
    ENDIF.
  ENDLOOP.
  DELETE tl_skb1 WHERE saknr  = ''.

  SORT tl_zsaldo_cta_moeda BY saknr.

  SELECT SINGLE bukrs land1
   FROM t001
   INTO wa_t001
   WHERE bukrs = p_bukrs.

*  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.

  SELECT SINGLE land1 waers curin curha
    FROM t005
    INTO wa_t005
    WHERE land1 = wa_t001-land1.

  REFRESH it_contas.
*---> 05/07/2023 - Migração S4 - DL
  SORT tl_ska1 BY saknr.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT tl_skb1.
    CLEAR: tl_zsaldo_cta_moeda, tl_ska1.
    READ TABLE tl_ska1 WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
    IF ( tl_ska1-ktoks = 'YB04' ).
      READ TABLE tl_zsaldo_cta_moeda WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
      IF sy-subrc = 0 AND
        ( tl_zsaldo_cta_moeda-waers = wa_t005-curin OR
          tl_zsaldo_cta_moeda-waers = wa_t005-curha ).
        wa_contas-bukrs = tl_skb1-bukrs.
        wa_contas-saknr = tl_skb1-saknr.
        APPEND wa_contas TO it_contas.
      ENDIF.
    ELSEIF  ( tl_zsaldo_cta_moeda-waers = wa_t005-curin OR
              tl_zsaldo_cta_moeda-waers = wa_t005-curha ).
      wa_contas-bukrs = tl_skb1-bukrs.
      wa_contas-saknr = tl_skb1-saknr.
      APPEND wa_contas TO it_contas.
**-US 156673-12-11-2024-#156673-RJF-Inicio
*    ELSEIF ( tl_ska1-ktoks = 'YB01' ).
*      wa_contas-bukrs = tl_skb1-bukrs.
*      wa_contas-saknr = tl_skb1-saknr.
*      APPEND wa_contas TO it_contas.
**-US 156673-12-11-2024-#156673-RJF-Fim
    ENDIF.

  ENDLOOP.

  SELECT *
    FROM skat
    INTO TABLE tl_skat
    FOR ALL ENTRIES IN tl_skb1
    WHERE spras = sy-langu
    AND   ktopl	=	'0050'
    AND   saknr = tl_skb1-saknr.

  CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
    EXPORTING
      ryear         = p_budat+0(4)
      contas        = it_contas
      p_gerar_todas = 'X'
    TABLES
      it_saldos     = it_saldo_contas
      it_saldos_2   = it_saldo_contas_2
      it_saldos_3   = it_saldo_contas_3
    EXCEPTIONS
      moeda_nao_adm = 1
      erro_ledger   = 2
      OTHERS        = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH it_contas2.
*---> 05/07/2023 - Migração S4 - DL
  SORT tl_ska1 BY saknr.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT tl_skb1.
    CLEAR: tl_zsaldo_cta_moeda, tl_ska1.
    READ TABLE tl_ska1 WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
    IF ( tl_ska1-ktoks = 'YB04' ).
      READ TABLE tl_zsaldo_cta_moeda WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
      IF sy-subrc = 0 AND
        ( tl_zsaldo_cta_moeda-waers NE wa_t005-curin AND
          tl_zsaldo_cta_moeda-waers NE wa_t005-curha AND
          tl_zsaldo_cta_moeda-waers NE wa_t005-waers ).
        wa_contas2-bukrs = tl_skb1-bukrs.
        wa_contas2-saknr = tl_skb1-saknr.
        APPEND wa_contas2 TO it_contas2.
      ENDIF.
    ELSEIF  ( tl_zsaldo_cta_moeda-waers NE wa_t005-curin AND
              tl_zsaldo_cta_moeda-waers NE wa_t005-curha AND
              tl_zsaldo_cta_moeda-waers NE wa_t005-waers ).
      wa_contas2-bukrs = tl_skb1-bukrs.
      wa_contas2-saknr = tl_skb1-saknr.
      APPEND wa_contas2 TO it_contas2.
**-US 156673-12-11-2024-#156673-RJF-Inicio
*    ELSEIF ( tl_ska1-ktoks = 'YB01' ).
*      wa_contas2-bukrs = tl_skb1-bukrs.
*      wa_contas2-saknr = tl_skb1-saknr.
*      APPEND wa_contas2 TO it_contas2.
**-US 156673-12-11-2024-#156673-RJF-Fim
    ENDIF.

  ENDLOOP.

  IF it_contas[] IS INITIAL AND it_contas2[] IS INITIAL.
    MESSAGE TEXT-032 TYPE 'I'. "Não foram encontradas contas configuradas
    EXIT.
  ENDIF.

  IF it_contas2[] IS NOT INITIAL.
    CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
      EXPORTING
        p_dt_posicao = vg_last_day
        contas       = it_contas2
      TABLES
        it_bsxs      = it_bsxs.
  ENDIF.
*---> 05/07/2023 - Migração S4 - DL
  SORT it_bsxs BY hkont waers.
*<--- 05/07/2023 - Migração S4 - DL
  LOOP AT tl_skb1.

    READ TABLE tl_ska1 WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
    IF ( tl_ska1-ktoks NE 'YB04' ).
*    IF ( tl_ska1-ktoks NE 'YB04' AND tl_ska1-ktoks NE 'YB01' ). "RJF -US 156673-12-11-2024-#156673-RJF
      DELETE it_bsxs WHERE hkont  = tl_skb1-saknr AND waers EQ  wa_t005-waers.
    ENDIF.
  ENDLOOP.

  SELECT *
    INTO TABLE t_zgl012_aux
   FROM zgl012_avm
  FOR ALL ENTRIES IN tl_skb1
  WHERE bukrs   EQ  tl_skb1-bukrs
    AND st_rev  EQ 'S'
    AND dt_aval EQ p_budat
    AND belnr   EQ tl_skb1-saknr.

  REFRESH: it_zib_contabil_err, it_zib_contabil_chv.
  IF t_zgl012_aux[] IS NOT INITIAL.
    SELECT *
            FROM zib_contabil_err
            INTO TABLE it_zib_contabil_err
            FOR ALL ENTRIES IN t_zgl012_aux
            WHERE obj_key = t_zgl012_aux-obj_key.
    SORT it_zib_contabil_err BY obj_key.

    SELECT zib_contabil_chv~mandt
              zib_contabil_chv~obj_key
              zib_contabil_chv~belnr
              zib_contabil_chv~bukrs
              zib_contabil_chv~gjahr
              zib_contabil~bldat
         FROM zib_contabil_chv INNER JOIN zib_contabil
         ON zib_contabil~obj_key EQ zib_contabil_chv~obj_key
         INTO TABLE it_zib_contabil_chv
         FOR ALL ENTRIES IN t_zgl012_aux
         WHERE zib_contabil_chv~obj_key = t_zgl012_aux-obj_key.

    SORT it_zib_contabil_chv BY obj_key.
  ENDIF.

*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Início de Alteração
  CONSTANTS:
        c_zgl042_empresa TYPE tvarvc-name VALUE 'ZGL042_EMPRESA'.

  DATA:
        r_bukrs_excecao TYPE RANGE OF zgl012_avm-bukrs.

  SELECT sign,
         opti AS option,
         low,
         high
  INTO TABLE @r_bukrs_excecao
    FROM tvarvc
    WHERE name = @c_zgl042_empresa.
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Fim de Alteração

  SORT: tl_ska1           BY saknr,
        tl_skat           BY saknr,
        tl_skb1           BY saknr,
        it_saldo_contas   BY racct,
        it_saldo_contas_2 BY racct,
        it_bsxs           BY hkont,
        t_zgl012_aux      BY belnr moeda_atu.

  vmes = p_budat+4(2).
  IF vmes = 12.
    vmes = 15.
  ENDIF.

*-US 156673-06-01-2025-#156673-WBARBOSA
  it_bsxs_g[] = it_bsxs[].
  LOOP AT it_contas INTO DATA(ls_contas).
    APPEND VALUE #( hkont = ls_contas-saknr ) TO it_bsxs_g.
  ENDLOOP.
*-US 156673-06-01-2025-#156673-WBARBOSA

  REFRESH o_alv2.
  LOOP AT tl_skb1.
    READ TABLE tl_ska1 WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
    "

*-US 156673-12-11-2024-#156673-RJF-Inicio
    SORT it_bsxs_g[] BY hkont vbund.
    DELETE ADJACENT DUPLICATES FROM it_bsxs_g COMPARING hkont vbund.
    LOOP AT it_bsxs_g ASSIGNING FIELD-SYMBOL(<fs_bsxs_g>) WHERE hkont EQ tl_skb1-saknr.
*-US 156673-12-11-2024-#156673-RJF-fim

      wl_saldo_mi = 0.
      READ TABLE it_saldo_contas WITH KEY racct  = tl_skb1-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas-slvt TO wl_saldo_mi.
        DO vmes TIMES VARYING refe1 FROM it_saldo_contas-sl01 NEXT it_saldo_contas-sl02.
          ADD refe1 TO wl_saldo_mi.
        ENDDO.
      ENDIF.

      "
      wl_saldo_mi2 = 0.
      READ TABLE it_saldo_contas_2 WITH KEY   racct  = tl_skb1-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas_2-slvt TO wl_saldo_mi2.
        DO vmes TIMES VARYING refe1 FROM it_saldo_contas_2-sl01 NEXT it_saldo_contas_2-sl02.
          ADD refe1 TO wl_saldo_mi2.
        ENDDO.
      ENDIF.


      wl_saldo_mi3 = 0.
      READ TABLE it_saldo_contas_3 WITH KEY   racct  = tl_skb1-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas_3-slvt TO wl_saldo_mi3.
        DO vmes TIMES VARYING refe1 FROM it_saldo_contas_3-sl01 NEXT it_saldo_contas_3-sl02.
          ADD refe1 TO wl_saldo_mi3.
        ENDDO.
      ENDIF.

      wl_saldo_doc = 0.
*    LOOP AT it_bsxs WHERE hkont  = tl_skb1-saknr. " -US 156673-12-11-2024-#156673-RJF
      LOOP AT it_bsxs WHERE hkont  = tl_skb1-saknr AND vbund = <fs_bsxs_g>-vbund. " -US 156673-12-11-2024-#156673-RJF
        IF it_bsxs-shkzg = 'S'. "debito
          ADD it_bsxs-dmbtr TO wl_saldo_mi.
          ADD it_bsxs-dmbe2 TO wl_saldo_mi2.
          ADD it_bsxs-dmbe3 TO wl_saldo_mi3.
          ADD it_bsxs-wrbtr TO wl_saldo_doc.
        ELSE.
          SUBTRACT it_bsxs-dmbtr FROM wl_saldo_mi.
          SUBTRACT it_bsxs-dmbe2 FROM wl_saldo_mi2.
          SUBTRACT it_bsxs-dmbe3 FROM wl_saldo_mi3.
          SUBTRACT it_bsxs-wrbtr FROM wl_saldo_doc.
        ENDIF.
      ENDLOOP.

      IF p_bukrs = '0101'. "PYG
        MULTIPLY wl_saldo_mi BY 100.
      ENDIF.

      "Sem saldo a corrigir
      IF wl_saldo_doc = 0 AND wl_saldo_mi = 0 AND wl_saldo_mi2 = 0.
        CONTINUE.
      ENDIF.

      READ TABLE tl_zsaldo_cta_moeda WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.

      o_alv2-racct            = tl_skb1-saknr.
*    o_alv2-rassc            = ''.  " -US 156673-12-11-2024-#156673-RJF
      o_alv2-rassc            = <fs_bsxs_g>-vbund.  " -US 156673-12-11-2024-#156673-RJF

      READ TABLE tl_skat WITH KEY saknr = tl_skb1-saknr BINARY SEARCH.
      o_alv2-txt50            = tl_skat-txt50.

      IF ( tl_ska1-ktoks = 'YB04' ).
*    IF ( tl_ska1-ktoks = 'YB04' AND tl_ska1-ktoks = 'YB01' ). "RJF
        o_alv2-waers            = tl_zsaldo_cta_moeda-waers.
      ELSE.
        o_alv2-waers            = wa_t005-curha.
      ENDIF.

      o_alv2-ktoks            = tl_ska1-ktoks.
      o_alv2-curr1            = wl_saldo_mi.
      o_alv2-curr2            = wl_saldo_mi2.
      o_alv2-curr3            = wl_saldo_mi3.
      o_alv2-wrbtr            = wl_saldo_doc.

      IF p_bukrs = '0004' OR p_bukrs = '0201' OR p_bukrs = '0202' OR p_bukrs = '0037'. "Elimina moeda dolar
        IF o_alv2-waers  = c_usd.
          CONTINUE.
        ENDIF.
      ENDIF.


      IF o_alv2-waers = c_usd.
        w_loop = 1.
      ELSE.
        w_loop = 2.
      ENDIF.
      CLEAR: wl_saldo_mi,wl_saldo_doc.
      DO w_loop TIMES.
        IF sy-index = 1.
          w_moeda = ''.
          CLEAR xtx_usd_aux.
          READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = o_alv2-waers.
          IF p_bukrs = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
            IF tl_skb1-saknr+0(1) = '1'. "ativos (compra)
              READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = o_alv2-waers.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
          ENDIF.
          IF sy-subrc = 0.
            o_alv2-tx_usd = wa_tcurr-ukurs.
            xtx_usd_aux = wa_tcurr-ukurs.
          ENDIF.
        ELSE. "ler o USD
          READ TABLE  t_tcurr INTO wa_tcurr WITH KEY fcurr = o_alv2-waers. "alrs2
*        READ TABLE  T_TCURR_A INTO WA_TCURR WITH KEY FCURR =  WA_T005-WAERS. "alrs
          IF p_bukrs = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
            IF tl_skb1-saknr+0(1) = '1'. "ativos (compra)
              READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = o_alv2-waers.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
          ENDIF.
          IF sy-subrc = 0.
            xtx_usd = wa_tcurr-ukurs.
          ENDIF.

          w_moeda = c_usd.
*        READ TABLE  T_TCURR INTO WA_TCURR WITH KEY FCURR = C_USD.
          READ TABLE  t_tcurr_a INTO wa_tcurr WITH KEY fcurr =  wa_t005-waers. "alrs
          IF p_bukrs = '0101'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
            IF tl_skb1-saknr+0(1) = '1'. "ativos (compra)
              READ TABLE  t_tcurr_g INTO wa_tcurr WITH KEY fcurr = c_usd.
            ENDIF.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = tl_skb1-saknr
              IMPORTING
                output = tl_skb1-saknr.
          ENDIF.
          IF sy-subrc = 0.
            o_alv2-tx_usd = wa_tcurr-ukurs.
          ENDIF.
        ENDIF.

        IF wa_zgl012_avm-tx_fech LT 0.
          MULTIPLY wa_zgl012_avm-tx_fech BY -1.
        ENDIF.

*     "// Verifica se a Moeda do banco tl_zsaldo_cta_moeda é Igual a moeda da Empresa wa_t005
*     "// se for Igual não usamos o saldo da moeda do documento
*     "// caso Suiça IR058012
        CLEAR vl_moeda_ok.
        IF wa_t005-waers    EQ tl_zsaldo_cta_moeda-waers
           OR wa_t005-curin EQ tl_zsaldo_cta_moeda-waers
          OR wa_t005-curha  EQ tl_zsaldo_cta_moeda-waers.
          vl_moeda_ok = abap_true.
        ENDIF.
*     "// caso Suiça IR058012

        o_alv2-belnr            = ''.
        o_alv2-belnr_est        = ''.

        READ TABLE t_zgl012_aux WITH KEY belnr     = tl_skb1-saknr
                                         moeda_atu = w_moeda BINARY SEARCH.
        IF sy-subrc = 0.
          IF t_zgl012_aux-estorno = 'X'.
            o_alv2-obj_key_est  = t_zgl012_aux-obj_key.
          ELSE.
            o_alv2-obj_key = t_zgl012_aux-obj_key.
          ENDIF.
          CLEAR o_alv2-log .
          IF o_alv2-obj_key IS NOT INITIAL.
            o_alv2-log     = icon_message_warning_small.
          ENDIF.
          READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = t_zgl012_aux-obj_key BINARY SEARCH.
          IF sy-subrc = 0.
            IF t_zgl012_aux-estorno = 'X'.
              o_alv2-belnr_est  = wa_zib_contabil_chv-belnr.
              o_alv2-log        = icon_system_undo.
            ELSE.
              o_alv2-belnr     = wa_zib_contabil_chv-belnr.
              o_alv2-log       = icon_led_green.
            ENDIF.

          ELSE.
            READ TABLE it_zib_contabil_err INTO wa_zib_contabil_err WITH KEY obj_key = t_zgl012_aux-obj_key BINARY SEARCH.
            IF sy-subrc = 0.
              o_alv2-log = icon_incomplete.
            ENDIF.
          ENDIF.
        ELSE.
          o_alv2-obj_key          = ''.
          o_alv2-log              = ''.
        ENDIF.
        "
        IF w_moeda = ''.
          o_alv2-moeda_atu =  TEXT-a39.
        ELSE.
          o_alv2-moeda_atu =  TEXT-a40.
        ENDIF.

**********************************************************************
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Início de Alteração
        IF  p_bukrs         IN r_bukrs_excecao
        AND r_bukrs_excecao IS NOT INITIAL
        AND o_alv2-waers    EQ 'BRL'.

          IF o_alv2-moeda_atu EQ TEXT-a39. " 01-INTERNA

            o_alv2-wrbtr = wl_saldo_mi3.

          ENDIF.

        ENDIF.
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Fim de Alteração
        IF sy-index = 1. "Moeda Interna
          IF o_alv2-wrbtr  EQ 0 AND vl_moeda_ok IS NOT INITIAL.
            TRY.
                IF o_alv2-tx_usd LT 0.
                  MULTIPLY o_alv2-tx_usd BY -1.
                  o_alv2-saldo_corr       = ( o_alv2-curr2 / o_alv2-tx_usd ).
                ELSE.
                  o_alv2-saldo_corr       = ( o_alv2-curr2 * o_alv2-tx_usd ).
                ENDIF.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ELSE.
            TRY.
                IF o_alv2-tx_usd LT 0.
                  MULTIPLY o_alv2-tx_usd BY -1.
                  o_alv2-saldo_corr       = ( o_alv2-wrbtr / o_alv2-tx_usd ). "Saldo moeda documento
                ELSE.
                  o_alv2-saldo_corr       = ( o_alv2-wrbtr * o_alv2-tx_usd ).
                ENDIF.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ENDIF.
          wl_saldo_mi = o_alv2-saldo_corr.
          o_alv2-vlr_ajust = ( o_alv2-saldo_corr - o_alv2-curr1 ).
          IF p_bukrs = '0201' OR p_bukrs = '0202'.
            CONTINUE.
          ENDIF.
        ELSE. "USD
          o_alv2-curr1 = wl_saldo_mi.
          TRY.
              IF xtx_usd LT 0.
*              MULTIPLY O_ALV2-TX_USD BY -1.
                wl_saldo_doc        = ( o_alv2-wrbtr / xtx_usd ). "converte do documento para a moeda interna
              ELSE.
                wl_saldo_doc        = ( o_alv2-wrbtr * xtx_usd ).
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          " 06.03.2018 - Não troca sinal
*        IF WL_SALDO_DOC LT 0.
*          MULTIPLY WL_SALDO_DOC BY -1.
*        ENDIF.

          xtx_usd_a = o_alv2-tx_usd.
          TRY.
              IF o_alv2-tx_usd LT 0.
                MULTIPLY o_alv2-tx_usd BY -1. "ALRS CAROL
*              IF WL_SALDO_DOC LT 0.
*                MULTIPLY WL_SALDO_DOC BY -1. "ALRS CAROL
*              ENDIF.
                o_alv2-saldo_corr       = ( wl_saldo_doc / o_alv2-tx_usd ). "converte moeda interna para USD
** INICIO - RBRIBEIRO - 14.07.2025 - 2000047925 - STEFANINI
*              ELSEIF p_bukrs EQ '0200' AND o_alv2-waers EQ 'BRL'.
*                o_alv2-saldo_corr       = ( o_alv2-wrbtr * o_alv2-tx_usd ).
** INICIO - RBRIBEIRO - 14.07.2025 - 2000047925 - STEFANINI
              ELSE.

*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Início de Alteração
*                o_alv2-saldo_corr       = ( wl_saldo_doc * o_alv2-tx_usd ).
                IF  p_bukrs          IN r_bukrs_excecao
                AND r_bukrs_excecao  IS NOT INITIAL
                AND o_alv2-waers     EQ 'BRL'
                AND o_alv2-moeda_atu EQ TEXT-a40. " 02-FORTE

                  o_alv2-saldo_corr       = ( wl_saldo_doc / o_alv2-tx_usd ).

                ELSE.

                  o_alv2-saldo_corr       = ( wl_saldo_doc * o_alv2-tx_usd ).

                ENDIF.
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Fim de Alteração

              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.
          IF o_alv2-saldo_corr LT 0 AND o_alv2-curr2 GT 0.
            o_alv2-vlr_ajust = ( o_alv2-saldo_corr - ( o_alv2-curr2 * -1  ) ).
          ELSE.
            o_alv2-vlr_ajust = ( o_alv2-saldo_corr - o_alv2-curr2 ).
          ENDIF.

        ENDIF.

        "Pega valores da Saldo ZGLT101_SALDO, trata valores positivos e negativos e soma com o saldo total da alv - PSA


        TYPES: BEGIN OF ty_zglt101_saldo,
                 vlr_mi    TYPE zglt101_saldo-dmbtr,
                 vlr_me    TYPE zglt101_saldo-dmbe2,
                 vlr_doc   TYPE zglt101_saldo-wrbtr,
                 sinal_doc TYPE zglt101_saldo-shkzg,
                 sinal_mi  TYPE zglt101_saldo-shkzg_mi,
                 sinal_me  TYPE zglt101_saldo-shkzg_me,
               END OF ty_zglt101_saldo.

        DATA: vlr_aux        TYPE STANDARD TABLE OF ty_zglt101_saldo WITH HEADER LINE,
              dt_inicio      TYPE sy-datum,
              dt_fim         TYPE sy-datum,
              o_alv2_aux_mi  TYPE faglflext-hslvt,
              o_alv2_aux_me  TYPE faglflext-hslvt,
              o_alv2_aux_doc TYPE faglflext-hslvt,
              saldo_aux_mi   TYPE decfloat34,
              saldo_aux_me   TYPE decfloat34,
              saldo_aux_doc  TYPE decfloat34. "

        dt_inicio = p_budat2+0(6) && '01'.
        dt_fim = p_budat2.

        "Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA

        DATA(vlr_aux_mi) = o_alv2-curr1.
        DATA(vlr_aux_me) = o_alv2-curr2.
        DATA(vlr_aux_doc) = o_alv2-wrbtr.

        FREE: o_alv2_aux_mi,o_alv2_aux_me,o_alv2_aux_doc.

        IF sy-index = 1."Somnente executar regra na linha 1 psa


          FREE: saldo_aux_doc,saldo_aux_mi,saldo_aux_me,vlr_aux.
          CLEAR: vlr_aux-sinal_doc,vlr_aux-sinal_me,vlr_aux-sinal_mi, vlr_aux-vlr_doc,vlr_aux-vlr_me,vlr_aux-vlr_mi.

          SELECT shkzg_mi AS sinal_mi, dmbtr AS vlr_mi,
              shkzg_me AS sinal_me, dmbe2 AS vlr_me,
           shkzg AS sinal_doc, wrbtr AS vlr_doc
            FROM zglt101_saldo
            INTO CORRESPONDING FIELDS OF @vlr_aux
                WHERE bukrs = @tl_skb1-bukrs AND waers = @o_alv2-waers AND saknr = @tl_skb1-saknr.
          ENDSELECT.


          IF vlr_aux IS NOT INITIAL.

            FREE: o_alv2-curr1, o_alv2-curr2, o_alv2-wrbtr.

            IF vlr_aux-sinal_doc = 'H'.
              saldo_aux_doc = ( vlr_aux-vlr_doc ) * ( -1 ).
            ELSE.
              saldo_aux_doc = ( vlr_aux-vlr_doc ) * ( 1 ).

            ENDIF.

            IF vlr_aux-sinal_mi = 'H'.
              saldo_aux_mi = ( vlr_aux-vlr_mi ) * ( -1 ).
            ELSE.
              saldo_aux_mi = ( vlr_aux-vlr_mi ) * ( 1 ).
            ENDIF.

            IF vlr_aux-sinal_me = 'H'.
              saldo_aux_me = ( vlr_aux-vlr_me ) * ( -1 ).
            ELSE.
              saldo_aux_me = ( vlr_aux-vlr_me ) * ( 1 ).
            ENDIF.


            o_alv2_aux_mi = vlr_aux_mi + saldo_aux_mi.
            o_alv2_aux_me = vlr_aux_me + saldo_aux_me .
            o_alv2_aux_doc = vlr_aux_doc + saldo_aux_doc .

            o_alv2-curr1 = o_alv2_aux_mi.
            o_alv2-curr2 = o_alv2_aux_me.
            o_alv2-wrbtr = o_alv2_aux_doc.

          ELSE.

            o_alv2-curr1 = vlr_aux_mi.
            o_alv2-curr2 = vlr_aux_me.
            o_alv2-wrbtr = vlr_aux_doc.

          ENDIF.

        ELSE.

          o_alv2-curr1 = vlr_aux_mi.
          o_alv2-curr2 = vlr_aux_me.
          o_alv2-wrbtr = vlr_aux_doc.

        ENDIF.

        FREE: vlr_aux-sinal_doc,vlr_aux-sinal_me,vlr_aux-sinal_mi.

        "Verifica Data Archive - 115495 IR133267 - ZGL042 saldo bancário - Archive - PSA
        "Movido o bloco a baixo da 5151 para que os valores já estejam atualizado!

        IF sy-index = 1. "Moeda Interna
          IF o_alv2-wrbtr  EQ 0 AND vl_moeda_ok IS NOT INITIAL.
            TRY.
                o_alv2-tx_usd = xtx_usd_aux. "ALRS 02.01.2024
                IF o_alv2-tx_usd LT 0.
                  MULTIPLY o_alv2-tx_usd BY -1.
                  o_alv2-saldo_corr       = ( o_alv2-curr2 / o_alv2-tx_usd ).
                ELSE.
                  o_alv2-saldo_corr       = ( o_alv2-curr2 * o_alv2-tx_usd ).
                ENDIF.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ELSE.
            TRY.
                o_alv2-tx_usd = xtx_usd_aux. "ALRS 02.01.2024
                IF o_alv2-tx_usd LT 0.
                  MULTIPLY o_alv2-tx_usd BY -1.
                  o_alv2-saldo_corr       = ( o_alv2-wrbtr / o_alv2-tx_usd ). "Saldo moeda documento
                ELSE.
                  o_alv2-saldo_corr       = ( o_alv2-wrbtr * o_alv2-tx_usd ).
                ENDIF.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ENDIF.
          wl_saldo_mi = o_alv2-saldo_corr.
          o_alv2-vlr_ajust = ( o_alv2-saldo_corr - o_alv2-curr1 ).
          IF p_bukrs = '0201' OR p_bukrs = '0202'.
            CONTINUE.
          ENDIF.
        ELSE. "USD
          o_alv2-curr1 = wl_saldo_mi.
          TRY.
              IF xtx_usd LT 0.
*              MULTIPLY O_ALV2-TX_USD BY -1.
                wl_saldo_doc        = ( o_alv2-wrbtr / xtx_usd ). "converte do documento para a moeda interna
              ELSE.
                wl_saldo_doc        = ( o_alv2-wrbtr * xtx_usd ).
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          " 06.03.2018 - Não troca sinal
*        IF WL_SALDO_DOC LT 0.
*          MULTIPLY WL_SALDO_DOC BY -1.
*        ENDIF.

          TRY.
              o_alv2-tx_usd = xtx_usd_a.
              IF o_alv2-tx_usd LT 0.
                MULTIPLY o_alv2-tx_usd BY -1. "ALRS CAROL
*              IF WL_SALDO_DOC LT 0.
*                MULTIPLY WL_SALDO_DOC BY -1. "ALRS CAROL
*              ENDIF.
                o_alv2-saldo_corr       = ( wl_saldo_doc / o_alv2-tx_usd ). "converte moeda interna para USD
** INICIO - RBRIBEIRO - 14.07.2025 - 2000047925 - STEFANINI
*              ELSEIF p_bukrs EQ '0200' AND o_alv2-waers EQ 'BRL'.
*                o_alv2-saldo_corr       = ( o_alv2-wrbtr * o_alv2-tx_usd ).
** FIM - RBRIBEIRO - 14.07.2025 - 2000047925 - STEFANINI
              ELSE.
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Início de Alteração
*                o_alv2-saldo_corr       = ( wl_saldo_doc * o_alv2-tx_usd ).

                IF  p_bukrs          IN r_bukrs_excecao
                AND r_bukrs_excecao  IS NOT INITIAL
                AND o_alv2-waers     EQ 'BRL'
                AND o_alv2-moeda_atu EQ TEXT-a40. " 02-FORTE

                  o_alv2-saldo_corr       = ( wl_saldo_doc / o_alv2-tx_usd ).

                ELSE.

                  o_alv2-saldo_corr       = ( wl_saldo_doc * o_alv2-tx_usd ).

                ENDIF.
*** Stefanini - IR245865 - 30/07/2025 - LAZAROSR - Fim de Alteração

              ENDIF.

            CATCH cx_sy_zerodivide.
          ENDTRY.
          IF o_alv2-saldo_corr LT 0 AND o_alv2-curr2 GT 0.
            o_alv2-vlr_ajust = ( o_alv2-saldo_corr - ( o_alv2-curr2 * -1  ) ).
          ELSE.
            o_alv2-vlr_ajust = ( o_alv2-saldo_corr - o_alv2-curr2 ).
          ENDIF.

        ENDIF.


**********************************************************************

        APPEND o_alv2.
*        CLEAR O_ALV2. " AJUSTE BUG SOLTO 167398 / AOENNING.
      ENDDO.
      CLEAR o_alv2.    " " AJUSTE BUG SOLTO 167398 / AOENNING.
    ENDLOOP.          " -US 156673-12-11-2024-#156673-RJF
  ENDLOOP.

  SORT o_alv2 BY racct moeda_atu.

  " DELETE O_ALV2 WHERE VLR_AJUST  = 0 AND LOG     NE ICON_LED_GREEN.
ENDFORM.                    " F_CONTA2
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat2 .
  DATA: wl_curr1_aux  TYPE dd03p-scrtext_l,
        wl_curr2_aux  TYPE dd03p-scrtext_l,
        wl_curr3_aux  TYPE dd03p-scrtext_l,
        wl_saldo_cor  TYPE dd03p-scrtext_l,
        wl_saldo_cor2 TYPE dd03p-scrtext_l,
        wl_vlr_ajst   TYPE dd03p-scrtext_l,
        wl_vlr_ajst2  TYPE dd03p-scrtext_l.


  DATA i TYPE i.
  wa_afield-tabname     = 'O_ALV2'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

*STATUS
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'LOG'.
  wa_afield-icon          = 'X'.
  wa_afield-hotspot  = 'X'.
  wa_afield-scrtext_s = 'St.'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = 'X'.
  wa_afield-outputlen = 06.
  APPEND wa_afield TO it_fieldcat.
  CLEAR wa_afield-icon.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RACCT'.
  wa_afield-scrtext_s = TEXT-a07.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = 'X'.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RASSC'.
  wa_afield-scrtext_s = TEXT-a34.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = 'X'.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.
  CLEAR wa_afield-icon.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TXT50'.
  wa_afield-scrtext_s = TEXT-a04.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-outputlen = 25.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
  CLEAR:   wa_afield-ref_field ,  wa_afield-ref_table.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WAERS'.
  wa_afield-scrtext_s = TEXT-a10.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-outputlen = 10.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.
  CLEAR:   wa_afield-ref_field ,  wa_afield-ref_table.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'KTOKS'.
  wa_afield-scrtext_s = TEXT-a35.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 10.
  APPEND wa_afield TO it_fieldcat.

*moeda de atualização
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOEDA_ATU'.
  wa_afield-scrtext_s = TEXT-a38.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key           = ''.
  wa_afield-outputlen     = 15.
  APPEND wa_afield TO it_fieldcat.

*Saldo DOC
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'WRBTR'.
  wa_afield-scrtext_m = TEXT-a48.
  wa_afield-scrtext_l = TEXT-a48.
  wa_afield-scrtext_s = TEXT-a48.
  wa_afield-key           = ' '.
  wa_afield-do_sum        = ''.
  wa_afield-outputlen     = 18.
  APPEND wa_afield TO it_fieldcat.

  " Saldo Moeda Interna
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CURR1'.
  wa_afield-scrtext_s = TEXT-a49.
  wa_afield-scrtext_l = TEXT-a49.
  wa_afield-scrtext_m = TEXT-a49.
  wa_afield-outputlen = 18.
  wa_afield-key       = ''.
  APPEND wa_afield TO it_fieldcat.

  " Saldo moeda forte
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CURR2'.
  wa_afield-scrtext_s = TEXT-a50.
  wa_afield-scrtext_l = TEXT-a50.
  wa_afield-scrtext_m = TEXT-a50.
  wa_afield-key       = ''.
  wa_afield-outputlen = 18.
  APPEND wa_afield TO it_fieldcat.

  " taxa
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TX_USD'.
  wa_afield-scrtext_s = TEXT-a17.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-key       = ''.
  wa_afield-outputlen = 12.
  APPEND wa_afield TO it_fieldcat.

  " Saldo Corrigido
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'SALDO_CORR'.
  wa_afield-scrtext_s = TEXT-a51.
  wa_afield-scrtext_l = TEXT-a51.
  wa_afield-scrtext_m = TEXT-a51.
  wa_afield-key       = ''.
  wa_afield-outputlen = 18.
  APPEND wa_afield TO it_fieldcat.
  CLEAR: wa_afield-ref_field, wa_afield-ref_table.

  "Valor do Ajuste
  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_AJUST'.
  wa_afield-scrtext_s = TEXT-a52.
  wa_afield-scrtext_l = TEXT-a52.
  wa_afield-scrtext_m = TEXT-a52.
  wa_afield-key       = ''.
  wa_afield-outputlen = 18.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = TEXT-a05.
  wa_afield-scrtext_l = TEXT-a05.
  wa_afield-scrtext_m = TEXT-a05.
  wa_afield-key       = ''.
  wa_afield-hotspot   = 'X'.
  wa_afield-outputlen = 12.
  APPEND wa_afield TO it_fieldcat.
*
*  I = I + 1.
*  CLEAR WA_AFIELD.
*  WA_AFIELD-COL_POS       = I.
*  WA_AFIELD-FIELDNAME     = 'BELNR_EST'.
*  WA_AFIELD-SCRTEXT_S = TEXT-A37.
*  WA_AFIELD-SCRTEXT_L = TEXT-A37.
*  WA_AFIELD-SCRTEXT_M = TEXT-A37.
*  WA_AFIELD-KEY           = ' '.
*  WA_AFIELD-DO_SUM        = ''.
*  WA_AFIELD-HOTSPOT  = 'X'.
*  WA_AFIELD-OUTPUTLEN = 12.
*  APPEND WA_AFIELD TO IT_FIELDCAT.
ENDFORM.                    " F_ALV_FIELDCAT2
*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  IF p_e_lanc = 'X' .
    APPEND 'ESTORNO' TO fcode.
  ENDIF.
  IF  p_v_lanc = 'X'.
    APPEND 'ESTORNO' TO fcode.
    APPEND 'LANCAR' TO fcode.
    APPEND '&REFRESH' TO fcode.
  ENDIF.
  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.

  SET TITLEBAR  'ZFTITLE'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.

  ENDIF.

  IF NOT cl_grid IS INITIAL.
    PERFORM zf_alv_header USING '2'.
    CALL METHOD cl_grid->refresh_table_display
*      EXPORTING
*        IS_STABLE      =
*        I_SOFT_REFRESH =
*      EXCEPTIONS
*        FINISHED       = 1
*        OTHERS         = 2
      .
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        no_margins = 'X'.


    PERFORM zf_alv_header USING '1'.


    IF editcontainer IS INITIAL .
      CREATE OBJECT editcontainer
        EXPORTING
          container_name = 'HEADER'.
    ENDIF .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.

    CREATE OBJECT cl_grid
      EXPORTING
        i_parent      = cl_container_95
*       I_PARENT      = CL_CONTAINER
        i_appl_events = 'X'.


    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    wa_layout-zebra      = 'X'.
    wa_layout-no_rowmove = 'X'.
    wa_layout-no_rowins  = 'X'.
    wa_layout-no_rowmark = space.
    IF p_e_lanc = 'X'. "Se for estorno
      wa_layout-grid_title = 'Estorno de lançamentos'.
    ELSEIF p_c_lanc = 'X'.
      wa_layout-grid_title = 'Criação de lançamentos'.
    ELSE.
      CLEAR wa_layout-grid_title .
    ENDIF.
    wa_layout-sel_mode   = 'A'.
    wa_layout-cwidth_opt = ''.
    wa_layout-box_fname  = 'MARK'.

    "WG_SAVE             = 'X'.
*    WG_X_VARIANT-REPORT = SY-REPID.
    CLEAR wg_x_variant-report .

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = wg_x_variant
*       I_SAVE          = WG_SAVE
        is_layout       = wa_layout
      CHANGING
        it_fieldcatalog = it_fieldcat[]
*       IT_SORT         = I_SORT[]
        it_outtab       = o_alv2[].


    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->catch_hotspot      FOR cl_grid.
    SET HANDLER event_receiver->handle_top_of_page FOR cl_grid.

  ENDIF.

ENDMODULE.                 " STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  IF NOT cl_grid IS INITIAL.
    CALL METHOD cl_grid->dispatch
      EXPORTING
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    IF sy-ucomm IS INITIAL.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = is_stable.
    ENDIF.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH o_alv2.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '&REFRESH'.
      PERFORM f_conta2.
    WHEN 'LANCAR' OR 'ESTORNO'.
      wg_acao = sy-ucomm.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = p_bukrs
          i_data   = p_budat
*         I_DEP_RESP = VG_DEPTO
        IMPORTING
          e_status = e_status
          e_messa  = e_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.
      IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      IF  e_status = 'E'.
        MESSAGE e398(00) WITH e_messa.
      ENDIF.


      IF t_t030h[] IS INITIAL.
        SELECT ktopl hkont waers curtp lsbew lhbew FROM t030h
          INTO TABLE t_t030h
          FOR ALL ENTRIES IN o_alv2
          WHERE ktopl = c_0050 AND
                hkont = o_alv2-racct.
      ENDIF.

      REFRESH indrow.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = indrow.

      LOOP AT o_alv2.
        o_alv2-mark = ' '.
        MODIFY o_alv2.
      ENDLOOP.

      LOOP AT indrow INTO w_ind.
        READ TABLE o_alv2 INDEX w_ind-index.
        o_alv2-mark = 'X'.
        MODIFY o_alv2 INDEX w_ind-index.
      ENDLOOP.
      " BUG 160280 - BG - INICIOO
*      LOOP AT o_alv2 WHERE mark = 'X'.
*
*        DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN o_alv2[]
*                    WHERE ( racct = o_alv2-racct ) NEXT x = x + 1 ).
*
*        wl_tabix = sy-tabix.
*        IF o_alv2-log   IS INITIAL          "primeiro processamento
*          OR o_alv2-log = icon_incomplete   "Erro contabilização  - reprocessa
*          OR ( o_alv2-log = icon_system_undo AND  wg_acao = 'ESTORNO' ) "Feito o estorno - reprocessa
*          OR ( o_alv2-log = icon_led_green AND wg_acao = 'ESTORNO' ). "Se lançamento OK permite estorno
*
*          IF wg_acao = 'LANCAR' AND o_alv2-log NE icon_incomplete.
*            IF o_alv2-moeda_atu+0(2) = '01'.
*              wa_zgl012_avm-moeda_atu = ''.
*            ELSE.
*              wa_zgl012_avm-moeda_atu =  c_usd.
*            ENDIF.
*            SELECT SINGLE *
*              FROM  zgl012_avm
*              INTO  wa_zgl012_avm
*              WHERE bukrs     EQ  p_bukrs
*              AND   dt_aval   EQ p_budat
*              AND   belnr     EQ  o_alv2-racct
*              "and   VBUND      eq o_alv2-RASSC " BUG 160280 - BG
*              AND   moeda_atu EQ wa_zgl012_avm-moeda_atu
*              AND   obj_key   NE ''.
*            IF sy-subrc = 0.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*
*          PERFORM zf_lancar2 CHANGING o_alv2 wl_erro.
*          CLEAR: o_alv2-obj_key, o_alv2-obj_key_est.
*          IF wg_acao = 'ESTORNO'.
**            O_ALV2-OBJ_KEY_EST = VOBJ_KEY.
*            CLEAR o_alv2-obj_key.
*          ELSE.
*            o_alv2-obj_key     = vobj_key.
*          ENDIF.
*          o_alv2-log     = icon_message_warning_small.
*          MODIFY o_alv2 INDEX wl_tabix TRANSPORTING obj_key obj_key_est log.
*        ENDIF.
*      ENDLOOP.
      " BUG 160280 - BG - FIMM

      LOOP AT o_alv2 INTO DATA(wa_alv2) WHERE mark = 'X'.

        DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN o_alv2[]
                    WHERE ( racct = wa_alv2-racct ) NEXT x = x + 1 ).

        wl_tabix = sy-tabix.
        IF wa_alv2-log   IS INITIAL          "primeiro processamento
          OR wa_alv2-log = icon_incomplete   "Erro contabilização  - reprocessa
          OR ( wa_alv2-log = icon_system_undo AND  wg_acao = 'ESTORNO' ) "Feito o estorno - reprocessa
          OR ( wa_alv2-log = icon_led_green AND wg_acao = 'ESTORNO' ). "Se lançamento OK permite estorno

          IF wg_acao = 'LANCAR' AND wa_alv2-log NE icon_incomplete.
            IF wa_alv2-moeda_atu+0(2) = '01'.
              wa_zgl012_avm-moeda_atu = ''.
            ELSE.
              wa_zgl012_avm-moeda_atu =  c_usd.
            ENDIF.
            SELECT SINGLE *
              FROM  zgl012_avm
              INTO  wa_zgl012_avm
              WHERE bukrs     EQ  p_bukrs
              AND   dt_aval   EQ p_budat
              AND   belnr     EQ  wa_alv2-racct
              AND   moeda_atu EQ wa_zgl012_avm-moeda_atu
              AND   obj_key   NE ''.
            IF sy-subrc = 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          PERFORM zf_lancar2 CHANGING wa_alv2 wl_erro.
          CLEAR: wa_alv2-obj_key, wa_alv2-obj_key_est.
          IF wg_acao = 'ESTORNO'.
*            O_ALV2-OBJ_KEY_EST = VOBJ_KEY.
            CLEAR wa_alv2-obj_key.
          ELSE.
            wa_alv2-obj_key     = vobj_key.
          ENDIF.
          wa_alv2-log     = icon_message_warning_small.
          MODIFY o_alv2 INDEX wl_tabix TRANSPORTING obj_key obj_key_est log.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_LANCAR2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_O_ALV2  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM zf_lancar2 CHANGING p_alv LIKE o_alv2
                         p_erro.

  DATA: wl_data(10),
        v_stblg     TYPE bkpf-stblg,
        tabix       TYPE sy-tabix,
        v_moeda_atu TYPE zgl012_avm-moeda_atu,
        v_contador  TYPE i.

  WRITE vg_last_day TO wl_data.

  SELECT SINGLE bukrs land1
    FROM t001
    INTO wa_t001
    WHERE bukrs = p_bukrs.

  "'US'
*  IF P_BUKRS = '0004'.
*    WA_T001-LAND1 = 'US'.
*  ENDIF.


  SELECT SINGLE land1 waers
    FROM t005
    INTO wa_t005
    WHERE land1 = wa_t001-land1.

** BUG - 173812 - Inicio - CBRAND
*  IF o_alv2-moeda_atu+0(2) = '01'.
*    CLEAR v_moeda_atu.
*  ELSE.
*    v_moeda_atu = c_usd.
*  ENDIF.
  IF p_alv-moeda_atu+0(2) = '01'.
    CLEAR v_moeda_atu.
  ELSE.
    v_moeda_atu = c_usd.
  ENDIF.
** BUG - 173812 - Fim - CBRAND

  REFRESH  it_zib_contabil.
  IF wg_acao = 'ESTORNO'.
    CLEAR: v_stblg, p_erro.
    vobj_key = p_alv-obj_key.

    SUBMIT z_fb08_zgl042 WITH p_obj = p_alv-obj_key
    AND RETURN.

    SELECT SINGLE stblg
      FROM zib_contabil_chv
    INNER JOIN bkpf
      ON  bkpf~bukrs = zib_contabil_chv~bukrs
      AND bkpf~belnr = zib_contabil_chv~belnr
      AND bkpf~gjahr = zib_contabil_chv~gjahr
        INTO v_stblg
      WHERE zib_contabil_chv~obj_key = p_alv-obj_key.

    IF v_stblg IS INITIAL. "Não estornou
      p_erro = 'X'.
      EXIT.
    ENDIF.
    DELETE FROM zgl012_avm WHERE obj_key = p_alv-obj_key.
    COMMIT WORK.
    EXIT.
  ELSE.
    "BUG 160280 - BG - INICIO
    "do LV_LINES TIMES.
*    LOOP AT o_alv2[] into data(w_alv2) WHERE RACCT eq o_alv2-RACCT.
*

*    DO 2 TIMES.
*      IF sy-index = 1.
*        CALL FUNCTION 'NUMBER_GET_NEXT'
*          EXPORTING
*            nr_range_nr = '01'
*            object      = 'ZID_GL'
*          IMPORTING
*            number      = vseq.
*        vnum = vseq .
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = vnum
*          IMPORTING
*            output = vnum.
*        CONCATENATE 'ZGL012' vnum  p_spmon+0(4) INTO wa_zib_contabil-obj_key.
*        vobj_key = wa_zib_contabil-obj_key.
*      ENDIF.
*
*      vseqitem = sy-index.
*      wa_zib_contabil-seqitem   = vseqitem.
*      IF p_alv-vlr_ajust  < 0.
*        wa_zib_contabil-xblnr = TEXT-a58. "'Receita'.
*      ELSE.
*        wa_zib_contabil-xblnr = TEXT-a59. "'Despesa'.
*      ENDIF.
*
*      IF sy-index = 1.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-bschl      = '50'.
*        ELSE.
*          wa_zib_contabil-bschl      = '40'.
*        ENDIF.
*      ELSE.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-bschl      = '40'.
*        ELSE.
*          wa_zib_contabil-bschl      = '50'.
*        ENDIF.
*      ENDIF.
*
*      CLEAR wa_zib_contabil-bupla.
*      IF p_bukrs = '0100'.
*        wa_zib_contabil-gsber = 'T001'.
*      ELSEIF p_bukrs = '0101'.
*        wa_zib_contabil-gsber = 'F101'.
*      ELSEIF p_bukrs = '0200'.
*        wa_zib_contabil-gsber = 'S201'.
*      ELSEIF p_bukrs = '0201'.
*        wa_zib_contabil-gsber = 'H201'.
*      ELSEIF p_bukrs = '0202'.
*        wa_zib_contabil-gsber = 'H202'.
*      ELSEIF p_bukrs = '0203'.
*        wa_zib_contabil-gsber = 'L203'.
*      ELSE.
*        CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-gsber.
*        CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-bupla.
*      ENDIF.
*
*      wa_zib_contabil-bukrs     = p_bukrs.
*      wa_zib_contabil-interface = '35'.
*      CONCATENATE p_spmon+4(2) p_spmon+0(4) INTO wa_zib_contabil-bktxt SEPARATED BY '.'.
*      wa_zib_contabil-bldat     = wl_data.
*      wa_zib_contabil-budat     = wl_data.
*      wa_zib_contabil-gjahr     = p_spmon+0(4).
*      wa_zib_contabil-monat     = p_spmon+4(2).
*      wa_zib_contabil-blart     = 'VC'.
*      IF sy-index = 1.
*        wa_zib_contabil-hkont     = p_alv-racct.
*      ELSE.
*        READ TABLE t_t030h INTO wa_t030h
*        WITH KEY hkont = p_alv-racct.
*        IF p_alv-vlr_ajust LT 0.
*          wa_zib_contabil-hkont     = wa_t030h-lsbew. "despesa
*        ELSE.
*          wa_zib_contabil-hkont     = wa_t030h-lhbew. "receita
*        ENDIF.
*      ENDIF.
*
*      wa_zib_contabil-VBUND = p_alv-RASSC. " BUG 160280 - BG
*
**      CONCATENATE 'Var. Monetária-'  P_BUDAT+4(2) '/' P_BUDAT+0(4) INTO WA_ZIB_CONTABIL-SGTXT.
*      CONCATENATE TEXT-a56  p_budat+4(2) '/' p_budat+0(4) INTO wa_zib_contabil-sgtxt.
*
*      wa_zib_contabil-wrbtr   = 0.
*      wa_zib_contabil-waers   = p_alv-waers.
*      wa_zib_contabil-waers_i = 'X'.
*
*      IF o_alv2-moeda_atu+0(2) = '01'. "Interna
*        wa_zib_contabil-dmbtr     = p_alv-vlr_ajust.
*        IF wa_zib_contabil-dmbtr LT 0.
*          MULTIPLY wa_zib_contabil-dmbtr BY -1.
*        ENDIF.
*        wa_zib_contabil-waers_f   = 'USD'.
*        wa_zib_contabil-dmbe2     = 0.
*      ELSE.
*        wa_zib_contabil-dmbtr     = 0.
*        wa_zib_contabil-waers_f   = 'USD'.
*        wa_zib_contabil-dmbe2     = p_alv-vlr_ajust.
*        IF wa_zib_contabil-dmbe2 LT 0.
*          MULTIPLY wa_zib_contabil-dmbe2 BY -1.
*        ENDIF.
*      ENDIF.
*
*      wa_zib_contabil-rg_atualizado  = 'N'.
*      wa_zib_contabil-bewar     = ''. "WL_BEWAR.
*      wa_zib_contabil-bktxt     = sy-uname.
*
*      APPEND wa_zib_contabil TO it_zib_contabil.
*    ENDDO.
*      ENDLOOP.
    "

    v_contador = 1.
*** BUG - 173812 - Inicio - CBRAND
    "LOOP AT O_ALV2[] INTO DATA(W_ALV2) WHERE RACCT EQ P_ALV-RACCT .
    LOOP AT o_alv2[] INTO DATA(w_alv2) WHERE racct EQ p_alv-racct AND moeda_atu = p_alv-moeda_atu.
*** BUG - 173812 - Fim - CBRAND
      DO 2 TIMES.
        IF v_contador = 1.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZID_GL'
            IMPORTING
              number      = vseq.
          vnum = vseq .

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = vnum
            IMPORTING
              output = vnum.
          CONCATENATE 'ZGL012' vnum  p_spmon+0(4) INTO wa_zib_contabil-obj_key.
          vobj_key = wa_zib_contabil-obj_key.
        ENDIF.

        vseqitem = v_contador.
        wa_zib_contabil-seqitem   = vseqitem.
        IF w_alv2-vlr_ajust  < 0.
          wa_zib_contabil-xblnr = TEXT-a58. "'Receita'.
        ELSE.
          wa_zib_contabil-xblnr = TEXT-a59. "'Despesa'.
        ENDIF.

        IF sy-index = 1.
          IF w_alv2-vlr_ajust LT 0.
            wa_zib_contabil-bschl      = '50'.
          ELSE.
            wa_zib_contabil-bschl      = '40'.
          ENDIF.
        ELSE.
          IF w_alv2-vlr_ajust LT 0.
            wa_zib_contabil-bschl      = '40'.
          ELSE.
            wa_zib_contabil-bschl      = '50'.
          ENDIF.
        ENDIF.

        CLEAR wa_zib_contabil-bupla.
        IF p_bukrs = '0100'.
          wa_zib_contabil-gsber = 'T001'.
        ELSEIF p_bukrs = '0101'.
          wa_zib_contabil-gsber = 'F101'.
        ELSEIF p_bukrs = '0200'.
          wa_zib_contabil-gsber = 'S201'.
        ELSEIF p_bukrs = '0201'.
          wa_zib_contabil-gsber = 'H201'.
        ELSEIF p_bukrs = '0202'.
          wa_zib_contabil-gsber = 'H202'.
        ELSEIF p_bukrs = '0203'.
          wa_zib_contabil-gsber = 'L203'.
        ELSE.
          CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-gsber.
          CONCATENATE p_bukrs+2(2) '01' INTO wa_zib_contabil-bupla.
        ENDIF.

        wa_zib_contabil-bukrs     = p_bukrs.
        wa_zib_contabil-interface = '35'.
        CONCATENATE p_spmon+4(2) p_spmon+0(4) INTO wa_zib_contabil-bktxt SEPARATED BY '.'.
        wa_zib_contabil-bldat     = wl_data.
        wa_zib_contabil-budat     = wl_data.
        wa_zib_contabil-gjahr     = p_spmon+0(4).
        wa_zib_contabil-monat     = p_spmon+4(2).
        wa_zib_contabil-blart     = 'VC'.
        IF sy-index = 1.
          wa_zib_contabil-hkont     = w_alv2-racct.
        ELSE.
          READ TABLE t_t030h INTO wa_t030h
          WITH KEY hkont = w_alv2-racct.
          IF w_alv2-vlr_ajust LT 0.
            wa_zib_contabil-hkont     = wa_t030h-lsbew. "despesa
          ELSE.
            wa_zib_contabil-hkont     = wa_t030h-lhbew. "receita
          ENDIF.
        ENDIF.

        wa_zib_contabil-vbund = w_alv2-rassc. " BUG 160280 - BG

*      CONCATENATE 'Var. Monetária-'  P_BUDAT+4(2) '/' P_BUDAT+0(4) INTO WA_ZIB_CONTABIL-SGTXT.
        CONCATENATE TEXT-a56  p_budat+4(2) '/' p_budat+0(4) INTO wa_zib_contabil-sgtxt.

        wa_zib_contabil-wrbtr   = 0.
        wa_zib_contabil-waers   = w_alv2-waers.
        wa_zib_contabil-waers_i = 'X'.
** BUG - 173812 - CBRAND - Inicio
**        IF o_alv2-moeda_atu+0(2) = '01'. "Interna
        IF  p_alv-moeda_atu+0(2) = '01'. "Interna
** BUG - 173812 - CBRAND - Fim
          wa_zib_contabil-dmbtr     = w_alv2-vlr_ajust.
          IF wa_zib_contabil-dmbtr LT 0.
            MULTIPLY wa_zib_contabil-dmbtr BY -1.
          ENDIF.
          wa_zib_contabil-waers_f   = 'USD'.
          wa_zib_contabil-dmbe2     = 0.
        ELSE.
          wa_zib_contabil-dmbtr     = 0.
          wa_zib_contabil-waers_f   = 'USD'.
          wa_zib_contabil-dmbe2     = w_alv2-vlr_ajust.
          IF wa_zib_contabil-dmbe2 LT 0.
            MULTIPLY wa_zib_contabil-dmbe2 BY -1.
          ENDIF.
        ENDIF.

        wa_zib_contabil-rg_atualizado  = 'N'.
        wa_zib_contabil-bewar     = ''. "WL_BEWAR.
        wa_zib_contabil-bktxt     = sy-uname.

        APPEND wa_zib_contabil TO it_zib_contabil.
        v_contador = v_contador + 1.
      ENDDO.
    ENDLOOP.
    " BUG 160280 - BG - FIM
    MODIFY zib_contabil FROM TABLE it_zib_contabil.
    REFRESH it_zib_contabil.

  ENDIF.

  "Grava Z
  IF p_alv-vlr_ajust NE 0.
    wa_zgl012_avm-bukrs             = p_bukrs.
    wa_zgl012_avm-dt_aval           = p_budat.
    wa_zgl012_avm-belnr             = p_alv-racct.
    wa_zgl012_avm-moeda_atu         = v_moeda_atu.
    wa_zgl012_avm-buzei             = 0.
    wa_zgl012_avm-bschl             = ''.
    wa_zgl012_avm-kunnr             = ''.
    wa_zgl012_avm-lifnr             = ''.
    wa_zgl012_avm-hkont             = ''.
    wa_zgl012_avm-vbund             = p_alv-rassc. " BUG 160280 - BG
    wa_zgl012_avm-umskz             = ''.
    wa_zgl012_avm-budat             = vg_last_day.
    wa_zgl012_avm-waers             = p_alv-waers.
    wa_zgl012_avm-gsber             = ''.
    wa_zgl012_avm-dmbtr             = 0.
    wa_zgl012_avm-wrbtr             = 0.
    wa_zgl012_avm-dmbe2             = 0.
    wa_zgl012_avm-kursf             = 0.
    wa_zgl012_avm-augdt             = ''.
    wa_zgl012_avm-augbl             = ''.
    wa_zgl012_avm-tx_fech           = 0.
    wa_zgl012_avm-vlr_atualizado    = 0.
    wa_zgl012_avm-vlr_acum_mes_ant  = 0.
    wa_zgl012_avm-vlr_acum_mes_atu  = 0.
    wa_zgl012_avm-vlr_variacao      = p_alv-vlr_ajust.
    wa_zgl012_avm-resultado         = ''.
    wa_zgl012_avm-dt_lcto           = ''.
    wa_zgl012_avm-doc_lcto          = ''.

    IF wg_acao = 'ESTORNO'.
      wa_zgl012_avm-estorno           = ''.
*      WA_ZGL012_AVM-ESTORNO           = 'X'.
      CLEAR vobj_key.
    ELSE.
      wa_zgl012_avm-estorno           = ''.
    ENDIF.
    wa_zgl012_avm-st_rev            = 'S'. "Marcar como "S" saldo conta

    MOVE: sy-uname TO wa_zgl012_avm-usnam,
          sy-uzeit TO wa_zgl012_avm-cputm,
          sy-datum TO wa_zgl012_avm-cpudt,
          vobj_key TO wa_zgl012_avm-obj_key.

    MODIFY zgl012_avm FROM wa_zgl012_avm.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT .
    ENDIF.
  ELSE.
    IF wg_acao = 'ESTORNO'.
*      WA_ZGL012_AVM-ESTORNO           = 'X'.
      wa_zgl012_avm-estorno           = ''.
      CLEAR vobj_key.
    ELSE.
      wa_zgl012_avm-estorno           = ''.
    ENDIF.
    UPDATE zgl012_avm SET obj_key = vobj_key
                          estorno = wa_zgl012_avm-estorno
                          usnam   = sy-uname
                          cputm   = sy-uzeit
                          cpudt   = sy-datum
    WHERE  bukrs             = p_bukrs
    AND    dt_aval           = p_budat
    AND    belnr             = p_alv-racct
    AND    moeda_atu         = v_moeda_atu.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT .
    ENDIF.
  ENDIF.

ENDFORM.                    "ZF_LANCAR2
*&---------------------------------------------------------------------*
*&      Form  EXEC_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ALV_OBJ_KEY  text
*----------------------------------------------------------------------*
*FORM EXEC_SHDB  USING    P_OBJ_KEY.
*  DATA: WL_SETLEAF  TYPE SETLEAF,
*          I_HEAD      TYPE TBTCJOB.
*
*  DATA:   WL_JOB_ID   LIKE TBTCJOB-JOBCOUNT.
*  DATA:   WL_JOBN(32).
*
*  DATA: BEGIN OF I_STEPLIST OCCURS 10.
*          INCLUDE STRUCTURE TBTCSTEP.
*  DATA: END OF I_STEPLIST.
*  DATA : C_NO(1) TYPE C . "value 'N', " Criação do job
*
*  DATA: WL_TBTCJOB  TYPE  TBTCJOB,
*        WL_TBTCSTRT TYPE  TBTCSTRT.
*
*  DATA: LV_REPNAME LIKE  RSVAR-REPORT.           " for variant handling
*  DATA: IV_VARNAME LIKE  RALDB-VARIANT VALUE 'SAP_UPGRADE'.
*  DATA: IV_VARIANTTEXT  LIKE  VARIT-VTEXT VALUE 'Upgrade variant'.
*  DATA: WL_SUBRC TYPE SY-SUBRC.
*  DATA: TT_REPORTPARAM TYPE TABLE OF  RSPARAMS WITH HEADER LINE.
*
*  SELECT SINGLE *
*   FROM SETLEAF
*   INTO WL_SETLEAF
*    WHERE SETNAME EQ 'MAGGI_JOB_USER'.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE TEXT-E01 TYPE 'E'.
*    EXIT.
*  ENDIF.
*  CONCATENATE 'Z_FB08_ZGL042' SY-TCODE  INTO WL_JOBN SEPARATED BY '|'.
*
*  I_HEAD-JOBNAME = WL_JOBN. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
*  I_HEAD-SDLSTRTTM = SY-UZEIT + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
*  I_HEAD-STEPCOUNT = 1.
*
*  TT_REPORTPARAM-SELNAME = 'P_OBJ'.
*  TT_REPORTPARAM-KIND =  'P'.
*  TT_REPORTPARAM-SIGN = 'I'.
*  TT_REPORTPARAM-OPTION = 'EQ'.
*  TT_REPORTPARAM-LOW = P_OBJ_KEY.
*  APPEND TT_REPORTPARAM.
*  CLEAR TT_REPORTPARAM.
*
*  LV_REPNAME = 'Z_FB08_ZGL042'.
**    Write the variant first (Insert or Update)
*  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
*    EXPORTING
*      IV_REPORTNAME         = LV_REPNAME
*      IV_VARIANTNAME        = IV_VARNAME
*      IV_VARIANTTEXT        = IV_VARIANTTEXT
*    IMPORTING
*      EV_FUNCRC             = WL_SUBRC
*    TABLES
*      TT_REPORTPARAM        = TT_REPORTPARAM
*    EXCEPTIONS
*      EXIST_CHECK_FAILED    = 1
*      UPDATE_FAILED         = 2
*      UPDATE_NOT_AUTHORIZED = 3
*      UPDATE_NO_REPORT      = 4
*      UPDATE_NO_VARIANT     = 5
*      UPDATE_VARIANT_LOCKED = 6
*      INSERT_FAILED         = 7
*      INSERT_NOT_AUTHORIZED = 8
*      INSERT_NO_REPORT      = 9
*      INSERT_VARIANT_EXISTS = 10
*      INSERT_VARIANT_LOCKED = 11
*      OTHERS                = 12.
*
*  I_STEPLIST-PARAMETER = IV_VARNAME. " Nome da variante
*  I_STEPLIST-PROGRAM = 'Z_FB08_ZGL042'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
*  I_STEPLIST-TYP = 'A'. " Tipo de Job
*  I_STEPLIST-AUTHCKNAM = WL_SETLEAF-VALFROM.
*  I_STEPLIST-LANGUAGE = SY-LANGU.
*  I_STEPLIST-ARCUSER = WL_SETLEAF-VALFROM.
*
*  APPEND I_STEPLIST.
*
*
*  C_NO = 'N'.
*  CALL FUNCTION 'BP_JOB_CREATE'
*    EXPORTING
*      JOB_CR_DIALOG       = C_NO " Coloque 'Y' se quiser ver
*      JOB_CR_HEAD_INP     = I_HEAD " os valores atribuidos
*    IMPORTING
*      JOB_CR_HEAD_OUT     = WL_TBTCJOB
*      JOB_CR_STDT_OUT     = WL_TBTCSTRT
*    TABLES
*      JOB_CR_STEPLIST     = I_STEPLIST
*    EXCEPTIONS
*      CANT_CREATE_JOB     = 1
*      INVALID_DIALOG_TYPE = 2
*      INVALID_JOB_DATA    = 3
*      JOB_CREATE_CANCELED = 4
*      OTHERS              = 5.
*
*  CALL FUNCTION 'JOB_CLOSE'
*    EXPORTING
*      JOBNAME   = WL_JOBN
*      JOBCOUNT  = WL_TBTCJOB-JOBCOUNT
*      STRTIMMED = 'X'.
*ENDFORM.                    " EXEC_SHDB
