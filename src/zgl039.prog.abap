*&---------------------------------------------------------------------*
*& Report  ZGL039
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zgl039.

TABLES: vbrk, vbrp, t001, t001w, tvak, bsis, mara.

TYPES: BEGIN OF ty_saida,
         bukrs      TYPE bsis-bukrs,
         butxt      TYPE t001-butxt,
         werks      TYPE vbrp-werks,
         name1      TYPE t001w-name1,
         aubel      TYPE vbrp-aubel,
         kunag      TYPE vbrk-kunag,
         name2      TYPE kna1-name1,
         matnr      TYPE vbrp-matnr,
         maktx      TYPE makt-maktx,
         matkl      TYPE mara-matkl,
         wgbez60    TYPE t023t-wgbez60,
         waerk      TYPE vbrk-waerk,
         lquant     TYPE bseg-dmbe2,
         lvlrtotfat TYPE bseg-dmbe2,
         vlrcontusd TYPE bseg-dmbe2,
         difusd     TYPE bseg-dmbe2,
         otdif      TYPE bseg-dmbe2,
       END OF ty_saida.

DATA: BEGIN OF tg_entrada_sd OCCURS 0,
        name1         TYPE kna1-name1,
        vkbur         TYPE vbak-vkbur,
        vbeln         TYPE vbak-vbeln,
        matnr         TYPE vbap-matnr,
        angnr         TYPE ekko-angnr,
        ihran         TYPE ekko-ihran,
        status,
        zterm(30),
        vtext         TYPE tvzbt-vtext,
        inco1         TYPE vbkd-inco1,
        inco2         TYPE vbkd-inco2,
        kurrf         TYPE vbkd-kurrf,
        descmat(60)   TYPE c,
        wrkst         TYPE mara-wrkst,
        ntgew         TYPE vbap-ntgew,
        qtefaturado   TYPE db20199vp,
        vlr_qtecont   TYPE konv-kbetr,
        vlr_qtefat    TYPE konv-kbetr,
        vlr_saldo     TYPE konv-kbetr,
        saldo         TYPE db20199vp,
        posnr         TYPE vbap-posnr,
        kwmeng        TYPE vbap-kwmeng,
        qtd           TYPE db20199vp,
        unid          TYPE vbap-vrkme,
        arktx         TYPE vbap-arktx,
        waers         TYPE konv-waers,
        kbetr         TYPE konv-kbetr,
        valdt         TYPE vbkd-valdt,
        lifsp         TYPE vbep-lifsp,
        erdat         TYPE vbak-erdat,
        safra         TYPE ajahr,
        cultura       TYPE acc_txtlg,
        doc_simulacao TYPE zsdt0041-doc_simulacao,
        vlr_frete     TYPE zsdt0041-vlr_frete,
        auart         TYPE vbak-auart,
        j_1bcfop      TYPE vbap-j_1bcfop,
        kbetr2        TYPE konv-kbetr,
        j_1btxsdc     TYPE vbap-j_1btxsdc.
DATA: END OF tg_entrada_sd.


DATA: BEGIN OF tg_entrada_ke24 OCCURS 0.
        INCLUDE STRUCTURE ce0magi.
      DATA: END OF  tg_entrada_ke24.


DATA: BEGIN OF tg_zglt041 OCCURS 0,
        tp_conta TYPE c.
        INCLUDE STRUCTURE zglt041.
      DATA:  END OF tg_zglt041.

DATA: BEGIN OF tg_setleaf OCCURS 0,
        cod_clas_not2 TYPE zglt041-cod_clas_not2.
        INCLUDE STRUCTURE setleaf.
      DATA:  END OF tg_setleaf.


DATA: BEGIN OF tg_setlinet OCCURS 0.
        INCLUDE STRUCTURE setlinet.
      DATA:  END OF tg_setlinet.

TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
       END OF ty_t001.

TYPES: BEGIN OF ty_t023t,
         matkl   TYPE t023t-matkl,
         spras   TYPE t023t-spras,
         wgbez60 TYPE t023t-wgbez60,
       END OF ty_t023t.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
       END OF ty_mara.

TYPES: BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt.


DATA: t_saida TYPE TABLE OF ty_saida.
DATA: t_t001w TYPE TABLE OF ty_t001w.
DATA: t_t001  TYPE TABLE OF ty_t001.
DATA: t_t023t TYPE TABLE OF ty_t023t.
DATA: t_mara  TYPE TABLE OF ty_mara.
DATA: t_makt  TYPE TABLE OF ty_makt.


DATA: w_saida TYPE ty_saida.
DATA: w_t001w TYPE ty_t001w.
DATA: w_t001  TYPE ty_t001.
DATA: w_t023t TYPE ty_t023t.
DATA: w_mara  TYPE ty_mara.
DATA: w_makt  TYPE ty_makt.


DATA: xquant      TYPE fkimg.
DATA: xvlrfat     TYPE fkimg.
DATA: xvlrtotfat  TYPE fkimg.
DATA: xvlrcontusd TYPE dmbe2,
      xcontusd    TYPE dmbe2.


DATA: it_fieldcat TYPE slis_t_fieldcat_alv,                   "Estrutura de saida
      it_event    TYPE slis_t_event       WITH HEADER LINE,   "Eventos
      it_header   TYPE kkblo_t_listheader WITH HEADER LINE,   "Cabeçalho
      vg_layout   TYPE slis_layout_alv,   "Layout do alv
      vg_ucomm    TYPE stree_ucomm,
      gs_variant  TYPE disvariant.

DATA: ano(4)      TYPE c,
      t_vbeln(20) TYPE c.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

CONSTANTS memoryid_epos(32) VALUE 'RKEB0601SEL_TAB'.


INITIALIZATION.

  SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS:
              p_bukrs FOR t001-bukrs OBLIGATORY, " empresa
              p_werks FOR t001w-werks,           " filial
              p_auart FOR tvak-auart OBLIGATORY, " Tp.Ordem Venda
              p_aubel FOR vbrp-aubel,            " Ordem Venda
              p_matnr FOR vbrp-matnr, " Material
              p_matkl FOR mara-matkl,   "grupo de mercadoria
              p_fatu  FOR bsis-budat OBLIGATORY,  " Data Faturamento
              p_entr  FOR bsis-budat OBLIGATORY.  " Data Entrada
  SELECTION-SCREEN END OF BLOCK a1.


START-OF-SELECTION.

  PERFORM busca_dados_zsdt0051.
  PERFORM busca_dados_ke24.
  PERFORM tratar_dados.
  PERFORM alv_estrutura_req.


FORM busca_dados_zsdt0051.

  DATA: it_rsparams TYPE TABLE OF rsparams,
        wa_rsparams TYPE  rsparams.

  CLEAR: it_rsparams[].

  wa_rsparams-selname = 'SD'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = 'X'.
  APPEND wa_rsparams TO it_rsparams.

  wa_rsparams-selname = 'MM'.
  wa_rsparams-kind    = 'P'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = ''.
  APPEND wa_rsparams TO it_rsparams.

  "TIPO DE CONTRATO
  IF p_auart[] IS NOT INITIAL.
    LOOP AT p_auart.
      wa_rsparams-selname = 'P_TPCONT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = p_auart-option.
      wa_rsparams-low     = p_auart-low.
      wa_rsparams-high    = p_auart-high.
      APPEND wa_rsparams  TO it_rsparams.
    ENDLOOP.
  ENDIF.

  "CONTRATO
  IF p_aubel[] IS NOT INITIAL.
    LOOP AT p_aubel.
      wa_rsparams-selname = 'P_CONT'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = p_aubel-option.
      wa_rsparams-low     = p_aubel-low.
      wa_rsparams-high    = p_aubel-high.
      APPEND wa_rsparams  TO it_rsparams.
    ENDLOOP.
  ENDIF.

  " ORGANIZAÇÃO DE VENDAS
  IF p_bukrs[] IS NOT INITIAL.
    LOOP AT p_bukrs.
      wa_rsparams-selname = 'P_ORGVEN'.
      wa_rsparams-kind    = 'S'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = p_bukrs-option.
      wa_rsparams-low     = p_bukrs-low.
      wa_rsparams-high    = p_bukrs-high.
      APPEND wa_rsparams TO it_rsparams.
    ENDLOOP.
  ENDIF.

  "CANAL DE DISTRIBUIÇÃO
  wa_rsparams-selname  = 'P_CDIST'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = 'EQ'.
  wa_rsparams-low      = '10'.
  APPEND wa_rsparams TO it_rsparams.

  "setor atividade
  wa_rsparams-selname = 'P_SATIV'.
  wa_rsparams-kind    = 'S'.
  wa_rsparams-sign    = 'I'.
  wa_rsparams-option  = 'EQ'.
  wa_rsparams-low     = '02'.	"Fertilizantes
  APPEND wa_rsparams TO it_rsparams.
  wa_rsparams-low     = '03'.	"Defensivos
  APPEND wa_rsparams TO it_rsparams.
  wa_rsparams-low     = '04'.	"Sementes
  APPEND wa_rsparams TO it_rsparams.

  "DATA ENTRADA
  wa_rsparams-selname  = 'P_DATENT'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = p_entr-option.
  wa_rsparams-low      = p_entr-low.
  wa_rsparams-high     = p_entr-high.
  APPEND wa_rsparams TO it_rsparams.

  "DATA FATURAMENTO
  wa_rsparams-selname  = 'P_FATUV'.
  wa_rsparams-kind     = 'S'.
  wa_rsparams-sign     = 'I'.
  wa_rsparams-option   = p_fatu-option.
  wa_rsparams-low      = p_fatu-low.
  wa_rsparams-high     = p_fatu-high.
  APPEND wa_rsparams TO it_rsparams.

  "CENTRO
  IF p_werks[] IS NOT INITIAL.
    LOOP AT p_werks.
      wa_rsparams-selname  = 'P_CENT'.
      wa_rsparams-kind     = 'S'.
      wa_rsparams-sign     = 'I'.
      wa_rsparams-option   = p_werks-option.
      wa_rsparams-low      = p_werks-low.
      wa_rsparams-high     = p_werks-high.
      APPEND wa_rsparams TO it_rsparams.
    ENDLOOP.
  ENDIF.
  "MATERIAL
  IF p_matnr[] IS NOT INITIAL.
    LOOP AT p_matnr.
      wa_rsparams-selname  = 'P_MATER'.
      wa_rsparams-kind     = 'S'.
      wa_rsparams-sign     = 'I'.
      wa_rsparams-option   = p_matnr-option.
      wa_rsparams-low      = p_matnr-low.
      wa_rsparams-high     = p_matnr-high.
      APPEND wa_rsparams TO it_rsparams.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-tabix
      text       = 'Extraindo dados em ZSDT0051(SD)...'.

  PERFORM f_prepare_run_time_info.

  SUBMIT zsdr0018 WITH SELECTION-TABLE it_rsparams AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR tg_entrada_sd.
      MOVE-CORRESPONDING <w_data> TO tg_entrada_sd.
      APPEND tg_entrada_sd.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM busca_dados_ke24.

  RANGES: r_spart FOR vbap-spart.

  DATA: BEGIN OF sel_tab OCCURS 50.
          INCLUDE STRUCTURE cedst.
        DATA: END OF sel_tab.

  CLEAR r_spart[].
  r_spart-sign    = 'I'.
  r_spart-option  = 'EQ'.
  r_spart-low     = '02'.
  APPEND r_spart.
  r_spart-low     = '03'.
  APPEND r_spart.
  r_spart-low     = '04'.
  APPEND r_spart.

  SELECT *
    FROM setleaf INTO CORRESPONDING FIELDS OF TABLE  tg_setleaf
    WHERE setname = 'MAGGI_KE24_INSUMOS'.

  IF tg_setleaf[] IS INITIAL.
    MESSAGE 'Set MAGGI_KE24_INSUMOS não parametrizado!' TYPE 'S'.
    STOP.
  ENDIF.

  LOOP AT tg_setleaf.
    tg_setleaf-cod_clas_not2 =  tg_setleaf-valfrom.
    MODIFY tg_setleaf.
  ENDLOOP.

  SELECT *
    FROM setlinet INTO TABLE tg_setlinet
    FOR ALL ENTRIES IN tg_setleaf
  WHERE setclass  EQ tg_setleaf-setclass
    AND subclass  EQ tg_setleaf-subclass
    AND setname   EQ tg_setleaf-setname
    AND lineid    EQ tg_setleaf-lineid.

  IF tg_setlinet[] IS INITIAL.
    MESSAGE  'Set MAGGI_KE24_INSUMOS (Texto Breve da Linha) não parametrizado!' TYPE 'S'.
    STOP.
  ENDIF.

  SELECT *
    FROM zglt041 INTO CORRESPONDING FIELDS OF TABLE tg_zglt041
    FOR ALL ENTRIES IN tg_setleaf
   WHERE bukrs         IN p_bukrs
    AND  cod_clas_not2 EQ tg_setleaf-cod_clas_not2.


  LOOP AT tg_zglt041.

    READ TABLE tg_setleaf WITH KEY cod_clas_not2 = tg_zglt041-cod_clas_not2.
    CHECK sy-subrc = 0.

    READ TABLE tg_setlinet WITH KEY setclass  = tg_setleaf-setclass
                                    subclass  = tg_setleaf-subclass
                                    setname   = tg_setleaf-setname
                                    lineid    = tg_setleaf-lineid.
    CHECK sy-subrc = 0.

    tg_zglt041-tp_conta =  tg_setlinet-descript.
    MODIFY tg_zglt041.
  ENDLOOP.

  CLEAR sel_tab[].

  sel_tab-fnam    = 'PERIO'.
  sel_tab-sign    = 'I'.
  sel_tab-option  = p_fatu-option.
  sel_tab-low     = p_fatu-low(4) && '0' && p_fatu-low+4(2).
  sel_tab-high    = p_fatu-high(4) && '0' && p_fatu-high+4(2).
  APPEND sel_tab.
  CLEAR sel_tab-high.

  LOOP AT tg_zglt041.
    sel_tab-fnam    = 'KSTAR'.
    sel_tab-sign    = 'I'.
    sel_tab-option  = 'EQ'.
    sel_tab-low     = tg_zglt041-saknr.
    APPEND sel_tab.
  ENDLOOP.

  sel_tab-fnam    = 'BUKRS'.
  sel_tab-sign    = 'I'.
  sel_tab-option  = p_bukrs-option.
  sel_tab-low     = p_bukrs-low.
  sel_tab-high    = p_bukrs-high.
  APPEND sel_tab.

  IF p_auart[] IS NOT INITIAL.
    LOOP AT p_auart.
      sel_tab-fnam    = 'AUART'.
      sel_tab-sign    = 'I'.
      sel_tab-option  = p_auart-option.
      sel_tab-low     = p_auart-low.
      sel_tab-high    = p_auart-high.
      APPEND sel_tab.
    ENDLOOP.
  ENDIF.

  IF p_matkl[] IS NOT INITIAL.
    LOOP AT p_matkl.
      sel_tab-fnam    = 'KMMAKL'.
      sel_tab-sign    = 'I'.
      sel_tab-option  = p_matkl-option.
      sel_tab-low     = p_matkl-low.
      sel_tab-high    = p_matkl-high.
      APPEND sel_tab.
    ENDLOOP.
  ENDIF.

  sel_tab-fnam    = 'ERKRS'.
  sel_tab-sign    = 'I'.
  sel_tab-option  = 'EQ'.
  sel_tab-low     = 'MAGI'.
  APPEND sel_tab.

  "Parametro Type - Account  = 2 / Ccosting = 1
  sel_tab-fnam    = 'PA_TYPE'.
  sel_tab-sign    = 'I'.
  sel_tab-option  = 'EQ'.
  sel_tab-low     = '2'.
  APPEND sel_tab.

  "Código Planejado/Real - 0 = Real / 1 - Planejado
  sel_tab-fnam    = 'PLIKZ'.
  sel_tab-sign    = 'I'.
  sel_tab-option  = 'EQ'.
  sel_tab-low     = '0'.
  APPEND sel_tab.

  EXPORT sel_tab TO MEMORY ID memoryid_epos.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-tabix
      text       = 'Extraindo dados em KE24...'.

  PERFORM f_prepare_run_time_info.

  SUBMIT rkeb0601 WITH SELECTION-TABLE sel_tab AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR tg_entrada_ke24.
      MOVE-CORRESPONDING <w_data> TO tg_entrada_ke24.
      APPEND tg_entrada_ke24.
    ENDLOOP.
  ENDIF.

ENDFORM.

FORM f_prepare_run_time_info.

  IF <t_data> IS ASSIGNED.
    CLEAR <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR <t_data_line>.
  ENDIF.

  FREE: l_data, l_data_line, l_data_descr, l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).
ENDFORM.

FORM f_get_runtime_info.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING r_data_descr = l_data_descr
                  r_data_line_descr = l_data_line_descr ).


      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data = <t_data>
                                                   t_data_line = <t_data_line> ).

    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->* TO <w_data>.
  ASSIGN l_data_line->* TO <w_data_line>.

ENDFORM.

FORM tratar_dados.
  DATA: it_vbeln TYPE RANGE OF vbfa-vbeln,
        wa_vbeln LIKE LINE OF it_vbeln.

  SELECT * FROM vbfa INTO TABLE @DATA(it_vbfa)
    FOR ALL ENTRIES IN @tg_entrada_sd
    WHERE vbelv   EQ @tg_entrada_sd-vbeln
    AND   vbtyp_n EQ 'H'.

  LOOP AT it_vbfa INTO DATA(wa_vbfa).
    wa_vbeln-sign   = 'I'.
    wa_vbeln-option = 'EQ'.
    wa_vbeln-low    =  wa_vbfa-vbeln.
    APPEND wa_vbeln TO it_vbeln.
    CLEAR wa_vbfa.
  ENDLOOP.

  LOOP AT tg_entrada_sd.

    w_saida-aubel =  tg_entrada_sd-vbeln.
    w_saida-matnr =  tg_entrada_sd-matnr.
    w_saida-maktx =  tg_entrada_sd-arktx.
    w_saida-name2 =  tg_entrada_sd-name1.
    w_saida-waerk =  tg_entrada_sd-waers.
    w_saida-lquant = tg_entrada_sd-qtefaturado.

    IF tg_entrada_sd-qtefaturado <> 0 OR
       tg_entrada_sd-vlr_qtefat <> 0.

      READ TABLE it_vbfa INTO wa_vbfa WITH KEY vbelv = tg_entrada_sd-vbeln.
      IF sy-subrc = 0.

        LOOP AT  tg_entrada_ke24 WHERE ( ( kaufn = tg_entrada_sd-vbeln OR  kaufn IN it_vbeln ) AND artnr = tg_entrada_sd-matnr ).

          SELECT SINGLE werks name1 FROM t001w INTO w_t001w
          WHERE werks EQ tg_entrada_ke24-werks.

          w_saida-werks = tg_entrada_ke24-werks.
          w_saida-name1 = w_t001w-name1.

          SELECT SINGLE bukrs butxt FROM t001 INTO w_t001
           WHERE bukrs EQ tg_entrada_ke24-bukrs.

          w_saida-bukrs = tg_entrada_ke24-bukrs.
          w_saida-butxt = w_t001-butxt.

          SELECT SINGLE matkl  spras  wgbez60 FROM t023t  INTO w_t023t
          WHERE matkl EQ tg_entrada_ke24-kmmakl
            AND spras EQ 'PT'.

          w_saida-matkl      = tg_entrada_ke24-kmmakl.
          w_saida-wgbez60    = w_t023t-wgbez60.
          w_saida-kunag      = tg_entrada_ke24-kndnr.

          xvlrcontusd = xvlrcontusd +  tg_entrada_ke24-wkgbtr.
        ENDLOOP.

      ELSE.
        LOOP AT  tg_entrada_ke24 WHERE (  kaufn = tg_entrada_sd-vbeln  AND artnr = tg_entrada_sd-matnr ).

          SELECT SINGLE werks name1 FROM t001w INTO w_t001w
          WHERE werks EQ tg_entrada_ke24-werks.

          w_saida-werks = tg_entrada_ke24-werks.
          w_saida-name1 = w_t001w-name1.

          SELECT SINGLE bukrs butxt FROM t001 INTO w_t001
           WHERE bukrs EQ tg_entrada_ke24-bukrs.

          w_saida-bukrs = tg_entrada_ke24-bukrs.
          w_saida-butxt = w_t001-butxt.

          SELECT SINGLE matkl  spras  wgbez60 FROM t023t  INTO w_t023t
          WHERE matkl EQ tg_entrada_ke24-kmmakl
            AND spras EQ 'PT'.

          w_saida-matkl      = tg_entrada_ke24-kmmakl.
          w_saida-wgbez60    = w_t023t-wgbez60.
          w_saida-kunag      = tg_entrada_ke24-kndnr.

          xvlrcontusd = xvlrcontusd +  tg_entrada_ke24-wkgbtr.
        ENDLOOP.

      ENDIF.


      IF xvlrcontusd < 0 .
        xcontusd =  xvlrcontusd * -1.
      ELSE.
        xcontusd =  xvlrcontusd.
      ENDIF.

      IF tg_entrada_sd-waers = 'USD'.
        xvlrtotfat    = tg_entrada_sd-vlr_qtefat.
      ELSE.
        IF tg_entrada_sd-kurrf <> 0.
          xvlrtotfat    = ( tg_entrada_sd-vlr_qtefat / tg_entrada_sd-kurrf ).
        ELSE.
          xvlrtotfat   =  tg_entrada_sd-vlr_qtefat.
        ENDIF.
      ENDIF.

      w_saida-lvlrtotfat = xvlrtotfat.
      w_saida-vlrcontusd = xcontusd.

      IF tg_entrada_sd-waers <> 'USD'.
        w_saida-difusd     = ( xvlrtotfat - xcontusd ).
      ENDIF.


      IF tg_entrada_sd-waers = 'USD'.
        w_saida-otdif = ( xvlrtotfat - xcontusd ).
      ELSE.
        w_saida-otdif = ( xvlrtotfat - xcontusd - w_saida-difusd ).
      ENDIF.

      IF tg_entrada_sd-waers <> 'USD'.
        w_saida-otdif = ( xvlrtotfat - xcontusd - w_saida-difusd ).
      ENDIF.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = w_saida-matnr
        IMPORTING
          output = w_saida-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = w_saida-kunag
        IMPORTING
          output = w_saida-kunag. " CLIENTE


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = w_saida-aubel
        IMPORTING
          output = w_saida-aubel. " ORDEM VENDA

      APPEND w_saida TO t_saida.


      CLEAR: w_saida,
             w_t001w,
             w_t001,
             w_t023t.

      xquant      = 0.
      xvlrfat     = 0.
      xvlrtotfat  = 0.
      xvlrcontusd = 0.
      xcontusd    = 0.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM alv_estrutura_req.
* Montar estruduta de ALV
  PERFORM f_alv_est.
* Monta cabeçalho
  PERFORM f_monta_cabecalho.
* Executa ALV
  PERFORM f_alv_executa .

ENDFORM.

FORM f_fieldcat USING p_cont p_key  p_tab  p_field p_desc
                      p_tam  p_qtde p_fix  p_just p_hot
             CHANGING p_fieldcat TYPE slis_t_fieldcat_alv.

* Tabela interna local
  DATA: tl_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  tl_fieldcat-col_pos    = p_cont. "Posição
  tl_fieldcat-key        = p_key.  "
  tl_fieldcat-tabname    = p_tab.  "Tabela interna
  tl_fieldcat-fieldname  = p_field."Campo
  tl_fieldcat-seltext_l  = p_desc. "Descrição longa
  tl_fieldcat-seltext_m  = p_desc. "Descrição media
  tl_fieldcat-seltext_s  = p_desc. "Descrição pequena
  tl_fieldcat-outputlen  = p_tam.  "Tamanho
  tl_fieldcat-quantity   = p_qtde. "Campo quantidade
  tl_fieldcat-fix_column = p_fix.  "Fixar coluna
  tl_fieldcat-just       = p_just. "Alinhar
  tl_fieldcat-hotspot    = p_hot.  "Clique chama evento
  APPEND tl_fieldcat TO p_fieldcat.

ENDFORM.

FORM f_alv_est.

  PERFORM f_fieldcat USING:

       '00' '' 'T_SAIDA'  'BUKRS'      'Empresa'                   7  ''  ''     '' ''   CHANGING it_fieldcat,
       '01' '' 'T_SAIDA'  'BUTXT'      'Nome Empresa'             25  ''  ''     '' ''   CHANGING it_fieldcat,
       '02' '' 'T_SAIDA'  'WERKS'      'Filial'                    6  ''  ''     '' ''   CHANGING it_fieldcat,
       '03' '' 'T_SAIDA'  'NAME1'      'Nome da Filial'           25  ''  ''     '' ''   CHANGING it_fieldcat,
       '04' '' 'T_SAIDA'  'AUBEL'      'Ordem Venda'              15  ''  ''     '' 'X'  CHANGING it_fieldcat,
       '05' '' 'T_SAIDA'  'KUNAG'      'Cliente'                  10  ''  ''     '' ''   CHANGING it_fieldcat,
       '06' '' 'T_SAIDA'  'NAME2'      'Nome do Cliente'          25  ''  ''     '' ''   CHANGING it_fieldcat,
       '07' '' 'T_SAIDA'  'MATNR'      'Material'                 12  ''  ''     '' ''   CHANGING it_fieldcat,
       '08' '' 'T_SAIDA'  'MAKTX'      'Descrição Material'       40  ''  ''     '' ''   CHANGING it_fieldcat,
       '09' '' 'T_SAIDA'  'MATKL'      'Gpo. Merc.'               10  ''  ''     '' ''   CHANGING it_fieldcat,
       '10' '' 'T_SAIDA'  'WGBEZ60'    'Descr. Gpo. Mercadoria'   30  ''  ''     '' ''   CHANGING it_fieldcat,
       '11' '' 'T_SAIDA'  'WAERK'      'Moeda'                    10  ''  ''     '' ''   CHANGING it_fieldcat,
       '12' '' 'T_SAIDA'  'LQUANT'     'Quantidade'               10  ''  ''     '' ''   CHANGING it_fieldcat,
       '13' '' 'T_SAIDA'  'LVLRTOTFAT' 'Valor. Fat. USD'          15  ''  ''     '' ''   CHANGING it_fieldcat,
       '14' '' 'T_SAIDA'  'VLRCONTUSD' 'Valor. Cont. USD'         15  ''  ''     '' ''   CHANGING it_fieldcat,
       '15' '' 'T_SAIDA'  'DIFUSD'     'Diferença Câmbio'         15  ''  ''     '' ''   CHANGING it_fieldcat,
       '15' '' 'T_SAIDA'  'OTDIF'      'Outras Dif.'              15  ''  ''     '' ''   CHANGING it_fieldcat.

ENDFORM.

FORM f_monta_cabecalho.

  DATA: vl_butxt        LIKE t001-butxt,  "Nome da empresa
        vl_filial       LIKE t001w-name1, "Nome da filial
        vl_data1(10)    TYPE c,
        vl_data2(10)    TYPE c,
        vl_data(25)     TYPE c,
        vl_dt1(10)      TYPE c,
        vl_dt2(10)      TYPE c,
        vl_data_l(25)   TYPE c,
        vl_filial2(100) TYPE c,
        vl_hkont(25),
        vl_ltext2(200)  TYPE c.

  CLEAR it_header.
  it_header-typ  = 'H'.
  it_header-info = 'Reconciliação Insumos Faturamento X Contábil'.
  APPEND  it_header.

  SELECT SINGLE butxt
   FROM t001
   INTO vl_butxt
  WHERE bukrs = p_bukrs-low.
  CONCATENATE p_bukrs-low
              vl_butxt INTO vl_butxt
              SEPARATED BY space.

  it_header-typ  = 'S'.
  it_header-key  = 'Empresa'.
  it_header-info = vl_butxt.
  APPEND  it_header.


  IF p_werks IS NOT INITIAL.
    SELECT SINGLE name1
      FROM t001w
      INTO vl_filial
     WHERE werks EQ p_werks-low.

    CONCATENATE p_werks-low  vl_filial INTO vl_filial2   SEPARATED BY space.

    it_header-typ  = 'S'.
    it_header-key  = 'Filial'.
    it_header-info = vl_filial2.
    APPEND  it_header.
  ENDIF.

  it_header-typ  = 'S'.
  it_header-key  = 'Tipo Ordem Venda'.
  it_header-info = p_auart-low.
  APPEND  it_header.


  IF p_aubel IS NOT INITIAL.
    it_header-typ  = 'S'.
    it_header-key  = 'Ordem Venda'.
    it_header-info = p_aubel-low.
    APPEND  it_header.
  ENDIF.

  IF p_matnr IS NOT INITIAL.
    it_header-typ  = 'S'.
    it_header-key  = 'Material'.
    it_header-info = p_matnr-low.
    APPEND  it_header.
  ENDIF.

  CONCATENATE p_fatu-low+6(2) '.'
              p_fatu-low+4(2) '.'
              p_fatu-low(4)
              INTO vl_data1.

  CONCATENATE p_fatu-high+6(2) '.'
              p_fatu-high+4(2) '.'
              p_fatu-high(4)
              INTO vl_data2.

  CONCATENATE vl_data1  'a'   vl_data2 INTO vl_data   SEPARATED BY space.

  it_header-typ  = 'S'.
  it_header-key  = 'Data Faturamento'.
  it_header-info = vl_data.
  APPEND  it_header.


  CONCATENATE p_entr-low+6(2) '.'
              p_entr-low+4(2) '.'
              p_entr-low(4)
              INTO vl_dt1.

  CONCATENATE p_entr-high+6(2) '.'
              p_entr-high+4(2) '.'
              p_entr-high(4)
              INTO vl_dt2.

  CONCATENATE vl_dt1  'a'   vl_dt2 INTO vl_data_l   SEPARATED BY space.

  it_header-typ  = 'S'.
  it_header-key  = 'Data Entrada'.
  it_header-info = vl_data_l.
  APPEND  it_header.

ENDFORM.

FORM f_alv_executa .
* Variavel Local
  DATA: vl_repid LIKE sy-repid.
  DATA: yt_event TYPE  slis_t_event.

  vl_repid = sy-repid.

  it_event-name = slis_ev_top_of_page.
  it_event-form = slis_ev_top_of_page.
  APPEND it_event.

* Determinar a tabela de cores
  "VG_LAYOUT-ZEBRA               = 'X'.

  vg_layout-colwidth_optimize = abap_true.
  vg_layout-zebra = abap_true.

* Função para exibir o ALV
  PERFORM fill_gs_variant.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = vl_repid
      "I_CALLBACK_PF_STATUS_SET = 'F_STATUS'
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = vg_layout
      it_fieldcat             = it_fieldcat[]
      i_default               = 'A'
      i_save                  = 'A'
      it_events               = it_event[]
    TABLES
      t_outtab                = t_saida
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_ALV_EXECUTA

FORM fill_gs_variant.

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '1000'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.

FORM f_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STATUS'.
ENDFORM.

FORM top_of_page.
* Cabeçalho
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = c_logo
      it_list_commentary = it_header[].
  SET TITLEBAR 'INI'.
ENDFORM.


FORM user_command USING p_ucomm LIKE sy-ucomm
      p_field TYPE slis_selfield.

  IF p_field-fieldname = 'AUBEL'.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'VA03'
      EXCEPTIONS
        ok     = 1
        not_ok = 2.
    IF sy-subrc = 2.
      MESSAGE e077(s#) WITH 'VA03'.
    ENDIF.

    READ TABLE t_saida INTO w_saida INDEX p_field-tabindex.

    IF w_saida-aubel IS NOT INITIAL.
      SET PARAMETER ID 'AUN' FIELD w_saida-aubel.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.
