*$*$ -------------------------------------------------------------- *$*$
*$*$                    GRUPO ANDRÉ MAGGI                           *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Autor     : Robson Motta - BBKO Consulting                     *$*$
*$*$ Data      : 28/06/2009                                         *$*$
*$*$ Descrição : Rotina de Transferência do Saldo do Centro a Fixar *$*$
*$*$             para o Centro Fixo no final de cada mês e o retorno*$*$
*$*$             no primeiro dia do mês subsequente.                *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Data      :                                                    *$*$
*$*$ Autor     :                                                    *$*$
*$*$ Solicit.  :                                                    *$*$
*$*$ Chamado   :                                                    *$*$
*$*$ Descrição :                                                    *$*$
*$*$ Versão    :                                                    *$*$
*$*$ ---------------------------------------------------------------*$*$
REPORT  zmmr018
        MESSAGE-ID z01
        NO STANDARD PAGE HEADING
        LINE-COUNT 65(1)
        LINE-SIZE  133.

***********************************************************************
* DECLARAÇÃO DE TABELAS                                               *
***********************************************************************
TABLES: s004,
        marc,
        mchb,
        mara,
        t001,
        mard.

***********************************************************************
* DECLARAÇÃO TIPOS                                                    *
***********************************************************************
TYPE-POOLS: slis,
            icon.

TYPES: BEGIN OF type_alv,
         selkz,                           "CheckBox para seleção
         matnr      TYPE matnr,       "Material
         maktx      TYPE maktx,       "Texto breve do Material
         werks      TYPE werks_d,     "Centro Fixo
         name1      TYPE name1,       "Descrição do Centro Fixo
         werks2     TYPE werks_d,     "Centro a Fixar
         name1_2    TYPE name1,       "Descrição do Centro a fixar
         lgort      TYPE lgort_d,     "Depósito
         lgobe      TYPE lgobe,       "Denominação depósito
         clabs      TYPE labst,       "Quantidade transferência
         clabs2     TYPE labst,       "Quantidade centro a fixar
         charg      TYPE charg_d,
         status(4),
         color_cell TYPE lvc_t_scol,  " Cell color
       END OF type_alv,

       BEGIN OF type_material,
         matnr TYPE matnr,
         maktx TYPE maktx,
         spras TYPE spras,
       END   OF type_material,

       BEGIN OF type_centro,
         werks TYPE werks_d,
         name1 TYPE name1,
       END   OF type_centro,

       BEGIN OF type_deposito,
         matnr   TYPE matnr,
         werks   TYPE werks_d,
         lgort   TYPE lgort_d,
         clabs   TYPE labst,
         cspem   TYPE speme,
         lgobe   TYPE lgobe,
         charg   TYPE charg_d,
         clabs_s TYPE labst,
         soma    TYPE labst,
       END   OF type_deposito,

       BEGIN OF type_erro,
         matnr  TYPE matnr,
         dtlanc TYPE d,
         werks  TYPE werks_d,
         werks2 TYPE werks_d,
         mensa  TYPE bapi_msg,
       END   OF type_erro,


       BEGIN OF type_estoque,

         matnr TYPE mslbh-matnr,
         werks TYPE mslbh-werks,
         charg TYPE mslbh-charg,
         lblab TYPE mslbh-lblab,
       END OF type_estoque,


       BEGIN OF type_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         umlmc TYPE marc-umlmc,
         trame TYPE marc-trame,
       END OF type_marc,

       BEGIN OF type_zmmt0017,
         mandt          TYPE zmmt0017-mandt,
         matnr          TYPE zmmt0017-matnr,
         centro_a_fixar TYPE zmmt0017-centro_a_fixar,
         centro_fixo    TYPE zmmt0017-centro_fixo,
         lgort          TYPE zmmt0017-lgort,
       END OF type_zmmt0017,

       BEGIN OF type_j_1bbranch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE j_1bbranch-branch,
       END OF type_j_1bbranch,

       BEGIN OF type_lancamento,
         matnr          TYPE mchb-matnr,
         werks          TYPE mchb-werks,
         lgort          TYPE mchb-lgort,
         charg          TYPE mchb-charg,
         bwkey          TYPE mbewh-bwkey,
         lbkum          TYPE mbewh-lbkum,
         lbkum2         TYPE mbewh-lbkum,
         centro_a_fixar TYPE zmmt0017-centro_a_fixar,
       END OF type_lancamento.



TYPES: BEGIN OF type_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         cspem TYPE mchb-cspem,
         flag  TYPE c,
       END   OF type_mchb.



DATA: t_mchb                TYPE TABLE OF type_mchb,
      t_mchb_aux            TYPE TABLE OF type_mchb,
      t_mchbh               TYPE TABLE OF type_mchb,
      t_zmm_saldo_lote_aux  TYPE TABLE OF zmm_saldo_lote WITH HEADER LINE,
      t_zmm_saldo_lote_aux2 TYPE TABLE OF zmm_saldo_lote WITH HEADER LINE,
      t_zmm_saldo_lote_aux3 TYPE TABLE OF zmm_saldo_lote WITH HEADER LINE,
      t_zmm_saldo_lote      TYPE TABLE OF zmm_saldo_lote WITH HEADER LINE,

      "it_j_1bbranch type table of j_1bbranch initial size 0 with header line,
      it_j_1bbranch         TYPE TABLE OF type_j_1bbranch,
      it_j_1bbranch_aux     TYPE TABLE OF type_j_1bbranch,

      it_lancamento         TYPE TABLE OF type_lancamento,
      wa_lancamento         TYPE type_lancamento,


      wa_deposito           TYPE type_deposito,
      wa_deposito2          TYPE type_deposito,
      wa_deposito_aux       TYPE type_deposito,
      wa_deposito_aux2      TYPE type_deposito,
      wa_deposito_aux_soma  TYPE type_deposito,
      wa_estoque_aux        TYPE type_estoque,
      wa_estoque            TYPE type_estoque,
      wa_j_1bbranch         TYPE type_j_1bbranch,
      wa_j_1bbranch_aux     TYPE j_1bbranch,

      wa_mchb               TYPE type_mchb,
      wa_mchb_aux           TYPE type_mchb,

      wa_mchbh              TYPE type_mchb,

      wa_marc               TYPE type_marc,
      wa_zmmt0017           TYPE type_zmmt0017,
      wa_material           TYPE type_material,
      wa_centro             TYPE type_centro,
      wa_saida              TYPE type_alv,
      wa_color              TYPE lvc_s_scol,
      vg_land1              TYPE land1,
      vg_tabix              TYPE sy-tabix.

***********************************************************************
* DECLARAÇÃO DE TABELAS INTERNAS                                      *
***********************************************************************
DATA: yt_fieldcat    TYPE  slis_t_fieldcat_alv,
      yt_event       TYPE  slis_t_event,
      yt_listheader  TYPE  slis_t_listheader,
      wa_goodsmvtitm TYPE  bapi2017_gm_item_create,
      wa_goodsmvtaux TYPE  bapi2017_gm_item_create.

DATA: yt_goodsmvtitm TYPE  STANDARD TABLE OF bapi2017_gm_item_create
                              WITH HEADER LINE,
      yt_return      TYPE  STANDARD TABLE OF bapiret2
                              WITH HEADER LINE.

DATA: yt_saida             TYPE  STANDARD TABLE OF type_alv,
      yt_zmmt0017          TYPE  STANDARD TABLE OF type_zmmt0017
                              WITH HEADER LINE,

      yt_material          TYPE  STANDARD TABLE OF type_material
                              WITH HEADER LINE,
      yt_centro            TYPE  STANDARD TABLE OF type_centro
                              WITH HEADER LINE,
      yt_deposito          TYPE  STANDARD TABLE OF type_deposito WITH HEADER LINE,
      yt_deposito_aux      TYPE  STANDARD TABLE OF type_deposito WITH HEADER LINE,
      yt_deposito_aux2     TYPE  STANDARD TABLE OF type_deposito WITH HEADER LINE,
      yt_deposito_aux_soma TYPE  STANDARD TABLE OF type_deposito WITH HEADER LINE,

      yt_deposito2         TYPE  STANDARD TABLE OF type_deposito
                              WITH HEADER LINE,
      yt_bapierro          TYPE  STANDARD TABLE OF type_erro
                              WITH HEADER LINE,
      yt_estoque           TYPE STANDARD TABLE OF type_estoque
                              WITH HEADER LINE,
      yt_estoque_aux       TYPE STANDARD TABLE OF type_estoque
                              WITH HEADER LINE,

      yt_marc              TYPE STANDARD TABLE OF type_marc WITH HEADER LINE,

      it_color             TYPE TABLE OF lvc_s_scol.

***********************************************************************
* DECLARAÇÃO DE ESTRUTURAS                                            *
***********************************************************************
DATA: e_alv_layout   TYPE  slis_layout_alv,
      e_fieldcat     TYPE  slis_fieldcat_alv,
      e_event        TYPE  slis_alv_event,

      e_goodsmvthedr TYPE  bapi2017_gm_head_01,
      e_goodsmvtcode TYPE  bapi2017_gm_code.

DATA: vd_data_inicial TYPE  d,
      vd_data_final   TYPE  d.

***********************************************************************
* DECLARAÇÃO DE CONSTANTES                                            *
***********************************************************************
CONSTANTS: cc_a(1)        TYPE c           VALUE 'A',
           cc_e(1)        TYPE c           VALUE 'E',
           cc_x(1)        TYPE c           VALUE 'X',

           cc_bwart_301   TYPE bwart       VALUE '301',
           cc_bwart_z99   TYPE bwart       VALUE 'Z99',
           cc_mb11_code06 TYPE gm_code     VALUE '06',
           cc_f_mb11      TYPE sy-ucomm    VALUE 'F_MB11',
           cc_bapi_rt(20) TYPE c
                              VALUE 'YT_RETURN-MESSAGE_V'.

***********************************************************************
* DECLARAÇÃO DE VARIÁVEIS                                             *
***********************************************************************
DATA: vc_repid           TYPE  sy-repid        VALUE sy-repid,
      vc_msgerr          TYPE  bapi_msg,
      vc_assign_bapi(30) TYPE  c,

      vc_data_cabec(10)  TYPE c,
      vn_cont_pagina(5)  TYPE n,

      lc_bukrs           LIKE  t001-bukrs,
      ltab_fields        LIKE  help_value OCCURS 0 WITH HEADER LINE,
      vc_formpfstatus    TYPE  slis_formname,
      vc_formusercomm    TYPE  slis_formname,
      vl_formtopofpag    TYPE  slis_formname,
      e_def_variant      TYPE  disvariant,
      e_variant          TYPE  disvariant.

***********************************************************************
* DEFINIÇÃO DA TELA DE SELEÇÃO                                        *
***********************************************************************

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_period TYPE s004-spmon OBLIGATORY,
              p_bukrs  TYPE t001-bukrs OBLIGATORY.
*                                     AS LISTBOX VISIBLE LENGTH 20
*                                     USER-COMMAND emp.
  SELECT-OPTIONS:
                  s_werks2  FOR marc-werks OBLIGATORY,
                  s_matnr   FOR mara-matnr OBLIGATORY,  "NO INTERVALS NO-EXTENSION,
                  s_lgort   FOR mard-lgort,
                  s_charg   FOR mchb-charg.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN BEGIN OF BLOCK b2
    WITH FRAME TITLE TEXT-002 NO INTERVALS.
    PARAMETERS:
      r_st_p LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X',
      r_st_n LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X'.
    PARAMETERS:
      r_es_t LIKE bsid-umskz AS CHECKBOX  DEFAULT ' '.
*  r_es_a LIKE bsid-umskz AS CHECKBOX  DEFAULT 'X'.
    PARAMETERS: p_vari  TYPE  disvariant-variant.
  SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN: END OF BLOCK b1.

***********************************************************************
* INITIALIZATION                                                      *
***********************************************************************
INITIALIZATION.

  vc_repid = sy-repid.
  PERFORM yf_alv_init_variant.

***********************************************************************
* AT SELECTION-SCREEN                                                 *
***********************************************************************
AT SELECTION-SCREEN.

* Display ALV variants.
  PERFORM yf_alv_display_variant.

***********************************************************************
* AT SELECTION-SCREEN ON                                              *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_period.
* MatchCode para período de seleção
  PERFORM yf_selecionar_mesano CHANGING p_period.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
* Variante ALV
  PERFORM yf_alv_reuse_variant_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bukrs.
  PERFORM yf_modifica.

*AT SELECTION-SCREEN OUTPUT.
*  PERFORM yf_modifica.

***********************************************************************
* START-OF-SELECTION                                                  *
***********************************************************************
START-OF-SELECTION.

  PERFORM: yf_obtem_dados_transferencia,
           yf_chama_processo_alv.

**********************************************************************
* Eventos geração de relatório
**********************************************************************
TOP-OF-PAGE.

  vn_cont_pagina = vn_cont_pagina + 1.

  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum
         INTO vc_data_cabec.

* Cabeçalho de Header
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  ULINE AT /1(132).
  WRITE: /01 sy-repid, 52  TEXT-r00, 123 vc_data_cabec.
  WRITE: /01 TEXT-003, 128 vn_cont_pagina.
  ULINE AT /1(132).

* Cabeçalho de Detalhes
  FORMAT COLOR COL_KEY INTENSIFIED OFF.

  WRITE: /01 TEXT-r01,
          20 TEXT-r02,
          31 TEXT-r03,
          43 TEXT-r04,
          55 TEXT-r05.

  ULINE AT /1(132). "(sy-linsz).

**********************************************************************
* LOGICA - Encerramento
**********************************************************************
END-OF-SELECTION.

  CHECK: NOT yt_bapierro[] IS INITIAL.
  PERFORM yf_gera_relatorio_erro.

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_TRANSFERENCIA
*&---------------------------------------------------------------------*
FORM yf_obtem_dados_transferencia.

* Log de erro
  REFRESH yt_bapierro.

  SELECT SINGLE land1
      INTO vg_land1
      FROM t001
      WHERE bukrs EQ p_bukrs.

* Cadastro dos centros Fixo e A Fixar
  SELECT mandt matnr centro_a_fixar centro_fixo lgort
    INTO TABLE yt_zmmt0017
    FROM zmmt0017
   WHERE matnr IN s_matnr
     AND centro_fixo    IN s_werks2
     AND lgort          IN s_lgort.

* Obtem dados adicionais de materiais
  PERFORM yf_obtem_dados_adicionais.

  IF yt_zmmt0017[] IS INITIAL.
    MESSAGE s000 DISPLAY LIKE cc_e
                 WITH TEXT-m01 TEXT-m02.
  ENDIF.


ENDFORM.                    " YF_OBTEM_DADOS_TRANSFERENCIA

*&---------------------------------------------------------------------*
*&      Form  YF_OBTEM_DADOS_ADICIONAIS
*&---------------------------------------------------------------------*
FORM yf_obtem_dados_adicionais.

  DATA: ln_ano   TYPE numc4,
        ln_mes   TYPE numc2,
        li_tabix TYPE i,
        v_charg  TYPE mchb-charg.

  CHECK NOT yt_zmmt0017[] IS INITIAL.

* Obtem o ano e o mes de seleção
  ln_mes = p_period+4(2).
  ln_ano = p_period(4).

  SELECT bukrs branch
   FROM j_1bbranch
   INTO TABLE it_j_1bbranch
     FOR ALL ENTRIES IN yt_zmmt0017
   WHERE branch EQ yt_zmmt0017-centro_fixo
   AND   bukrs  NE p_bukrs.

  IF it_j_1bbranch[] IS NOT INITIAL.
    MESSAGE s000 DISPLAY LIKE cc_e
            WITH TEXT-m04 p_bukrs.
    EXIT.
  ENDIF.
* Dados adicionais de material
  SELECT matnr maktx spras
    INTO TABLE yt_material
    FROM makt
      FOR ALL ENTRIES IN yt_zmmt0017
   WHERE matnr = yt_zmmt0017-matnr
     AND spras = sy-langu.

* Dados adicionais de centro
  SELECT werks name1
    INTO TABLE yt_centro
    FROM t001w
      FOR ALL ENTRIES IN yt_zmmt0017
   WHERE werks = yt_zmmt0017-centro_fixo.


  SORT yt_zmmt0017 BY matnr centro_fixo lgort.

  " ALRS novo
  SELECT  matnr werks lgort charg clabs cspem
    INTO TABLE t_mchb_aux
    FROM mchb
    FOR ALL ENTRIES IN yt_zmmt0017
    WHERE werks EQ yt_zmmt0017-centro_fixo
      AND matnr EQ yt_zmmt0017-matnr
      AND lgort EQ yt_zmmt0017-lgort
      AND charg IN s_charg.

  CHECK t_mchb_aux[] IS NOT INITIAL.


  CONCATENATE p_period '01' INTO vd_data_inicial.
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vd_data_inicial
    IMPORTING
      last_day_of_month = vd_data_final
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  REFRESH: t_zmm_saldo_lote, t_zmm_saldo_lote_aux.

  " PASSAR CENTROS para FUNCTION
  t_mchb[] = t_mchb_aux[].
  SORT t_mchb BY werks matnr.
  DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING werks matnr.
  LOOP AT t_mchb INTO wa_mchb.
    "Busca saldo centro fixo
    CLEAR t_zmm_saldo_lote_aux.
    t_zmm_saldo_lote_aux-werks = wa_mchb-werks.
    APPEND t_zmm_saldo_lote_aux.
    "Busca saldo centro a fixar
    READ TABLE yt_zmmt0017  WITH KEY matnr       = wa_mchb-matnr
                                     centro_fixo = wa_mchb-werks
                                     lgort       = wa_mchb-lgort  BINARY SEARCH.
    IF sy-subrc = 0.
      t_zmm_saldo_lote_aux-werks = yt_zmmt0017-centro_a_fixar.
      APPEND t_zmm_saldo_lote_aux.
    ENDIF.
  ENDLOOP.

  " PASSAR MATERIAL para FUNCTION
  t_mchb[] = t_mchb_aux[].
  SORT t_mchb BY matnr.
  DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING matnr.
  LOOP AT t_mchb INTO wa_mchb.
    "Busca saldo centro fixo
    CLEAR t_zmm_saldo_lote_aux.
    t_zmm_saldo_lote_aux-matnr = wa_mchb-matnr.
    APPEND t_zmm_saldo_lote_aux.
  ENDLOOP.


  " PASSAR DEPOSITOS para FUNCTION
  t_mchb[] = t_mchb_aux[].
  SORT t_mchb BY lgort.
  DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING lgort.
  LOOP AT t_mchb INTO wa_mchb.
    IF s_lgort IS NOT INITIAL.
      IF wa_mchb-lgort IN s_lgort.
        CLEAR t_zmm_saldo_lote_aux.
        t_zmm_saldo_lote_aux-lgort = wa_mchb-lgort.
        APPEND t_zmm_saldo_lote_aux.
      ENDIF.
    ELSE.
      READ TABLE yt_zmmt0017 WITH KEY lgort = wa_mchb-lgort.
      IF sy-subrc = 0.
        CLEAR t_zmm_saldo_lote_aux.
        t_zmm_saldo_lote_aux-lgort = wa_mchb-lgort.
*        t_zmm_saldo_lote_aux-matnr = wa_mchb-matnr. "ALRS
*        t_zmm_saldo_lote_aux-werks = wa_mchb-werks. "ALRS
        APPEND t_zmm_saldo_lote_aux.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF s_charg IS NOT INITIAL.
    " PASSAR LOTES para FUNCTION
    t_mchb[] = t_mchb_aux[].
    SORT t_mchb BY charg.
    DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING charg.
    LOOP AT t_mchb INTO wa_mchb .
      CLEAR t_zmm_saldo_lote_aux.
      t_zmm_saldo_lote_aux-charg = wa_mchb-charg.
      APPEND t_zmm_saldo_lote_aux.
    ENDLOOP.
  ENDIF.
























  CALL FUNCTION 'Z_BUSCA_SALDO_LOTE'
    EXPORTING
*     i_matnr          = t_zmm_saldo_lote_aux2-matnr " s_matnr-low "ALRS
      i_datum          = vd_data_final
    TABLES
      t_zmm_saldo_lote = t_zmm_saldo_lote_aux.

  APPEND LINES OF t_zmm_saldo_lote_aux TO t_zmm_saldo_lote.

  DELETE t_zmm_saldo_lote WHERE menge EQ 0.


*  t_zmm_saldo_lote_aux2[] = t_zmm_saldo_lote_aux[].
*  t_zmm_saldo_lote_aux3[] = t_zmm_saldo_lote_aux[].
*  DELETE t_zmm_saldo_lote_aux2 WHERE lgort EQ ''.
*  LOOP AT t_zmm_saldo_lote_aux2 WHERE lgort IS NOT INITIAL.
*    "
*    t_zmm_saldo_lote_aux[] = t_zmm_saldo_lote_aux3[].
*    DELETE t_zmm_saldo_lote_aux WHERE lgort NE ''.
*    APPEND t_zmm_saldo_lote_aux2 TO t_zmm_saldo_lote_aux.
*
*    CALL FUNCTION 'Z_BUSCA_SALDO_LOTE'
*      EXPORTING
**        i_matnr          = t_zmm_saldo_lote_aux2-matnr " s_matnr-low "ALRS
*        i_datum          = vd_data_final
*      TABLES
*        t_zmm_saldo_lote = t_zmm_saldo_lote_aux.
*
*    APPEND LINES OF t_zmm_saldo_lote_aux TO t_zmm_saldo_lote.
*
*    DELETE t_zmm_saldo_lote WHERE menge EQ 0.
*  ENDLOOP.

  SORT t_zmm_saldo_lote BY werks matnr charg lgort.

  LOOP AT t_mchb_aux INTO wa_mchb_aux.
    READ TABLE yt_zmmt0017  WITH KEY matnr       = wa_mchb_aux-matnr
                                     centro_fixo = wa_mchb_aux-werks
                                     lgort       = wa_mchb_aux-lgort  BINARY SEARCH. "ALRS
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    IF vg_land1 NE 'AR'.
      wa_mchb_aux-charg = wa_mchb_aux-charg(4). "tratar LOTE_FILIAL como safra
    ENDIF.
    "
    CLEAR: wa_lancamento.
    wa_lancamento-matnr = wa_mchb_aux-matnr.
    wa_lancamento-bwkey = wa_mchb_aux-werks.
    wa_lancamento-werks = wa_mchb_aux-werks.
    wa_lancamento-lgort = wa_mchb_aux-lgort.
    wa_lancamento-charg = wa_mchb_aux-charg.
    "Saldo centro fixo

    READ TABLE it_lancamento
     WITH KEY
       matnr = wa_mchb_aux-matnr
       bwkey = wa_mchb_aux-werks
       werks = wa_mchb_aux-werks
       lgort = wa_mchb_aux-lgort
       charg = wa_mchb_aux-charg
       TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    "Saldo Centro Fixo
    READ TABLE t_zmm_saldo_lote WITH KEY werks = wa_mchb_aux-werks
                                         matnr = wa_mchb_aux-matnr
                                         charg = wa_mchb_aux-charg
                                         lgort = wa_mchb_aux-lgort BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.
    IF r_st_p = '' AND t_zmm_saldo_lote-menge GT 0.
      CONTINUE.
    ENDIF.

    IF r_st_n = '' AND t_zmm_saldo_lote-menge LT 0.
      CONTINUE.
    ENDIF.

    wa_lancamento-lbkum = t_zmm_saldo_lote-menge.


    "Saldo centro a fixar
    v_charg = wa_mchb_aux-charg.
    IF v_charg GT '2019' AND vg_land1 NE 'AR'.
      CONCATENATE v_charg '_' wa_mchb_aux-werks INTO v_charg.
    ENDIF.
    wa_lancamento-centro_a_fixar = yt_zmmt0017-centro_a_fixar.
    READ TABLE t_zmm_saldo_lote WITH KEY werks = yt_zmmt0017-centro_a_fixar
                                         matnr = wa_mchb_aux-matnr
                                         charg = v_charg
                                         lgort = wa_mchb_aux-lgort BINARY SEARCH.
    IF sy-subrc = 0.
      wa_lancamento-lbkum2 = t_zmm_saldo_lote-menge.
    ENDIF.

    APPEND wa_lancamento TO it_lancamento.
    CLEAR: wa_lancamento.
  ENDLOOP.

  SORT: t_mchb BY lgort charg  ASCENDING.

ENDFORM.                    " YF_OBTEM_DADOS_ADICIONAIS

*&---------------------------------------------------------------------*
*&      Form  YF_CHAMA_PROCESSO_ALV
*&---------------------------------------------------------------------*
FORM yf_chama_processo_alv.

  CHECK: it_j_1bbranch[] IS INITIAL.
  CHECK: NOT yt_zmmt0017[] IS INITIAL.

  PERFORM yf_monta_saida_alv.

  PERFORM yf_gera_laytgeral_alv.
  PERFORM yf_gera_cabecalho_alv.
  PERFORM yf_gera_linhasdet_alv.
  PERFORM yf_gera_rotevento_alv.

  PERFORM yf_exibe_relatorio_alv.

ENDFORM.                    " YF_CHAMA_PROCESSO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_SAIDA_ALV
*&---------------------------------------------------------------------*
FORM yf_monta_saida_alv.


  SORT: yt_material BY matnr,
        yt_centro   BY werks.
  LOOP AT it_lancamento INTO wa_lancamento.
    REFRESH it_color.
    wa_saida-matnr  = wa_lancamento-matnr.
    wa_saida-werks  = wa_lancamento-bwkey.
    wa_saida-werks2 = wa_lancamento-centro_a_fixar.
    wa_saida-lgort  = wa_lancamento-lgort.
    wa_saida-clabs  = wa_lancamento-lbkum. "fixo
    wa_saida-clabs2 = wa_lancamento-lbkum2. "fixar
    wa_saida-charg  = wa_lancamento-charg.

    READ TABLE yt_material INTO wa_material WITH KEY matnr = wa_lancamento-matnr BINARY SEARCH.
    wa_saida-maktx  = wa_material-maktx.

    READ TABLE yt_centro INTO wa_centro WITH KEY werks = wa_lancamento-bwkey BINARY SEARCH.
    wa_saida-name1  = wa_centro-name1.

    IF wa_saida-clabs LT 0.
      MOVE 'CLABS'     TO wa_color-fname.
      MOVE '6'         TO wa_color-color-col.
      MOVE '1'         TO wa_color-color-int.
      MOVE '1'         TO wa_color-color-inv.
      APPEND wa_color TO it_color.
    ENDIF.
    "STATUS
    IF wa_saida-clabs GT 0.
      wa_saida-status = icon_checked.
    ELSEIF wa_saida-clabs2 LT abs( wa_saida-clabs ) .
      wa_saida-status =  icon_incomplete.
    ELSEIF wa_saida-clabs2 GE abs( wa_saida-clabs ) .
      wa_saida-status =  icon_generate.
    ENDIF.
    wa_saida-color_cell[] = it_color[].
    APPEND wa_saida TO yt_saida.

    CLEAR: wa_lancamento, wa_material, wa_centro, wa_saida.
  ENDLOOP.

ENDFORM.                    " YF_MONTA_SAIDA_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LAYTGERAL_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_laytgeral_alv.

  CLEAR: e_alv_layout.
  e_alv_layout-default_item      = cc_x.
  e_alv_layout-zebra             = cc_x.
  e_alv_layout-colwidth_optimize = cc_x.
  e_alv_layout-box_fieldname    = 'SELKZ'.
  e_alv_layout-coltab_fieldname  = 'COLOR_CELL'.

ENDFORM.                    " YF_GERA_LAYTGERAL_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_CABECALHO_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_cabecalho_alv.

  DATA: lc_data(10)     TYPE c,
        lc_hora(10)     TYPE c,
        lc_dt(22)       TYPE c,
        lc_mandt(30)    TYPE c,
        lc_execucao(60) TYPE c.

  DATA: w_listheader LIKE LINE OF yt_listheader.

  WRITE sy-datum DD/MM/YYYY TO lc_data.
  WRITE sy-uzeit            TO lc_hora.

  CONCATENATE lc_data lc_hora INTO lc_dt SEPARATED BY ' / '.
  CONCATENATE sy-mandt sy-host sy-uname
         INTO lc_mandt SEPARATED BY ' - '.
  CONCATENATE lc_dt lc_mandt INTO lc_execucao SEPARATED BY space.

  REFRESH: yt_listheader.

  CLEAR w_listheader.
  w_listheader-typ     = 'S'.       "H=Header, S=Selection, A=Action
  w_listheader-key     = TEXT-003.
  APPEND w_listheader TO yt_listheader.

  CONCATENATE p_period+4(2) '/' p_period(4) INTO lc_data.

  w_listheader-typ    = 'S'.
  w_listheader-key    = TEXT-004.
  w_listheader-info   = lc_data.
  APPEND w_listheader TO yt_listheader.

  w_listheader-typ    = 'S'.
  w_listheader-key    = TEXT-005.
  w_listheader-info   = lc_execucao.
  APPEND w_listheader TO yt_listheader.

ENDFORM.                    " YF_GERA_CABECALHO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_LINHASDET_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_linhasdet_alv.

  DATA li_numcol   TYPE int4 VALUE 0.
  REFRESH: yt_fieldcat.

* Material
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-key                   = cc_x.
  e_fieldcat-fix_column            = cc_x.
  e_fieldcat-fieldname             = 'MATNR'.
  e_fieldcat-ref_tabname           = 'MARA'.
  e_fieldcat-ref_fieldname         = 'MATNR'.
  e_fieldcat-seltext_s             = 'Material'.
  e_fieldcat-seltext_m             = e_fieldcat-seltext_s.
  e_fieldcat-reptext_ddic          = 'Código do Material'.
  e_fieldcat-just                  = 'R'.
  e_fieldcat-outputlen             = 18.
  APPEND e_fieldcat TO yt_fieldcat.

* Texto Breve do Material
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'MAKTX'.
  e_fieldcat-ref_tabname           = 'MAKT'.
  e_fieldcat-ref_fieldname         = 'MAKTX'.
  e_fieldcat-seltext_l             = 'Texto Breve de Material'.
  e_fieldcat-ddictxt               = 'L'.
  e_fieldcat-reptext_ddic          = 'Texto curto do material'.
  e_fieldcat-outputlen             = 35.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.


* Descrição do Centro
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'NAME1'.
  e_fieldcat-ref_tabname           = 'T001W'.
  e_fieldcat-ref_fieldname         = 'NAME1'.
  e_fieldcat-seltext_l             = 'Denominação do Centro Fixo'.
  e_fieldcat-reptext_ddic          = 'Denominação do Centro Fixo'.
  e_fieldcat-ddictxt               = 'L'.
  e_fieldcat-outputlen             = 30.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.

* Depósito
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'LGORT'.
  e_fieldcat-ref_tabname           = 'MARD'.
  e_fieldcat-ref_fieldname         = 'LGORT'.
  e_fieldcat-seltext_s             = 'Depósito'.
  e_fieldcat-seltext_m             = e_fieldcat-seltext_s.
  e_fieldcat-reptext_ddic          = 'Depósito'.
  e_fieldcat-ddictxt               = 'S'.
  e_fieldcat-outputlen             = 8.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.

* Lote
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'CHARG'.
  e_fieldcat-ref_tabname           = 'MCHB'.
  e_fieldcat-ref_fieldname         = 'CHARG'.
  e_fieldcat-seltext_s             = 'Lote'.
  e_fieldcat-seltext_m             = e_fieldcat-seltext_s.
  e_fieldcat-reptext_ddic          = 'Lote'.
  e_fieldcat-ddictxt               = 'S'.
  e_fieldcat-outputlen             = 10.
  e_fieldcat-lowercase             = cc_x.
  APPEND e_fieldcat TO yt_fieldcat.

* Centro
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'WERKS'.
  e_fieldcat-seltext_m             = 'Centro Fixo'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Centro Fixo'.
  e_fieldcat-ddictxt               = 'S'.
  e_fieldcat-outputlen             = 15.
  APPEND e_fieldcat TO yt_fieldcat.

* Quantidade de transferência
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'CLABS'.
  e_fieldcat-ref_tabname           = 'MCHB'.
  e_fieldcat-ref_fieldname         = 'CLABS'.
  e_fieldcat-seltext_m             = 'Sdo Fixo'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Quantidade de transferência'.
  e_fieldcat-ddictxt               = 'M'.
  e_fieldcat-outputlen             = 18.
  APPEND e_fieldcat TO yt_fieldcat.

* Centro
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'WERKS2'.
  e_fieldcat-seltext_m             = 'Centro Fixar'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Centro Fixar'.
  e_fieldcat-ddictxt               = 'S'.
  e_fieldcat-outputlen             = 15.
  APPEND e_fieldcat TO yt_fieldcat.


* Quantidade de transferência
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'CLABS2'.
  e_fieldcat-ref_tabname           = 'MCHB'.
  e_fieldcat-ref_fieldname         = 'CLABS'.
  e_fieldcat-seltext_m             = 'Sdo a Fixar'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Quantidade de transferência'.
  e_fieldcat-ddictxt               = 'M'.
  e_fieldcat-outputlen             = 18.
  APPEND e_fieldcat TO yt_fieldcat.

* Status
  CLEAR e_fieldcat.
  ADD 1 TO li_numcol.
  e_fieldcat-col_pos               = li_numcol.
  e_fieldcat-fieldname             = 'STATUS'.
  e_fieldcat-seltext_m             = 'Status'.
  e_fieldcat-seltext_l             = e_fieldcat-seltext_m.
  e_fieldcat-reptext_ddic          = 'Status'.
  e_fieldcat-ddictxt               = 'M'.
  e_fieldcat-outputlen             = 8.
  e_fieldcat-icon                  = 'X'.
  e_fieldcat-hotspot               = 'X'.
  APPEND e_fieldcat TO yt_fieldcat.
ENDFORM.                    " YF_GERA_LINHASDET_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_ROTEVENTO_ALV
*&---------------------------------------------------------------------*
FORM yf_gera_rotevento_alv.

  REFRESH: yt_event.

  CLEAR e_event.
  e_event-name = slis_ev_top_of_list.
  e_event-form = 'YF_ALV_TOP_OF_LIST'.
  APPEND e_event TO yt_event.

  CLEAR e_event.
  e_event-name = slis_ev_user_command.
  e_event-form = 'YF_ALV_USER_COMMAND'.
  APPEND e_event TO yt_event.

ENDFORM.                    " YF_GERA_ROTEVENTO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_EXIBE_RELATORIO_ALV
*&---------------------------------------------------------------------*
FORM yf_exibe_relatorio_alv.

  vc_formpfstatus = 'YF_SET_PF_STATUS'.
  vc_formusercomm = 'YF_ALV_USER_COMMAND'.
  vl_formtopofpag = 'YF_ALV_TOP_OF_LIST'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_interface_check        = ' '
      i_callback_program       = vc_repid
      i_callback_pf_status_set = vc_formpfstatus
      i_callback_user_command  = vc_formusercomm
      i_callback_top_of_page   = vl_formtopofpag
      is_layout                = e_alv_layout
      it_fieldcat              = yt_fieldcat
      i_default                = 'X'
      i_save                   = cc_a
      is_variant               = e_variant
      it_events                = yt_event
    TABLES
      t_outtab                 = yt_saida[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " YF_EXIBE_RELATORIO_ALV

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_TOP_OF_LIST
*&---------------------------------------------------------------------*
FORM yf_alv_top_of_list.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = yt_listheader.

ENDFORM.                    " YF_ALV_TOP_OF_LIST

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_USER_COMMAND - Eventos
*&---------------------------------------------------------------------*
FORM yf_alv_user_command USING i_ucomm    LIKE sy-ucomm
                               i_selfield TYPE slis_selfield.

  DATA: lf_excbapi,
        linha TYPE sy-tfill.

  CASE i_ucomm.
    WHEN '&IC1'.
      IF i_selfield-fieldname = 'STATUS' AND i_selfield-value = icon_generate.
        linha = i_selfield-tabindex.
        PERFORM yf_transferencia_estoque USING linha CHANGING lf_excbapi.
        CLEAR linha.
      ENDIF.
*   Transferência de estoque
    WHEN  cc_f_mb11.
      linha = 0.
      PERFORM yf_transferencia_estoque USING linha CHANGING lf_excbapi.
    WHEN OTHERS.

  ENDCASE.

* Verifica se houve processamento de movimento encerra o ALV
  IF lf_excbapi = cc_x.
    IF ( yt_bapierro[] IS INITIAL ).
      MESSAGE s000 WITH TEXT-m03.
    ENDIF.
    i_selfield-refresh = cc_x.
*    I_SELFIELD-EXIT = CC_X.
  ENDIF.

ENDFORM.                    " YF_ALV_USER_COMMAND

*---------------------------------------------------------------------*
*       FORM YF_SET_PF_STATUS
*---------------------------------------------------------------------*
FORM yf_set_pf_status USING extab TYPE slis_t_extab.

  DATA: lc_fcode   TYPE gui_code.

  "Copiar (SE41) o pf-status do programa "SAPLKKBL/STANDARD"
  "Elimnar comandos que não necessite

* Funções a serem inibidas do PF-STATUS
*  lc_fcode = ?????????????.
*  APPEND lc_fcode TO extab.

  SET PF-STATUS 'PRINCIPAL_100' EXCLUDING extab.

ENDFORM.                    "YF_SET_PF_STATUS

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_INIT_VARIANT
*&---------------------------------------------------------------------*
FORM yf_alv_init_variant .

  CLEAR: e_variant,
         e_def_variant.

*  IF p_bukrs IS INITIAL.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'R_ES_A'.
**        screen-input = 0.
*        screen-invisible = 1.
**    ELSE.
**      screen-active = 0.
*      ENDIF.
*
*      IF screen-name EQ 'R_ES_T'.
**        screen-input = 1.
*        screen-invisible = 0.
**    ELSE.
**      screen-active = 1.
*      ENDIF.
*
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.

  e_variant-report = sy-repid.

* Get default variant at screen initialisation
  e_def_variant = e_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = 'A'
    CHANGING
      cs_variant = e_def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
    p_vari = e_def_variant-variant.
  ENDIF.

ENDFORM.                    " YF_ALV_INIT_VARIANT

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_DISPLAY_VARIANT
*&---------------------------------------------------------------------*
FORM yf_alv_display_variant.

* Display variant list on screen.
  IF NOT p_vari IS INITIAL.

    MOVE: e_variant TO e_def_variant,
          p_vari     TO e_def_variant-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = e_def_variant.

    e_variant = e_def_variant.
    CLEAR p_vari.

  ELSE.
    CLEAR e_variant.
    e_variant-report = vc_repid.
  ENDIF.

ENDFORM.                    " YF_ALV_DISPLAY_VARIANT

*&---------------------------------------------------------------------*
*&      Form  YF_ALV_REUSE_VARIANT_F4
*&---------------------------------------------------------------------*
FORM yf_alv_reuse_variant_f4 .

  DATA: lc_exit(1) TYPE c.

  CLEAR: lc_exit,
         e_def_variant.

* Call the ALV variant on F4 push button.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = e_variant
      i_save     = cc_a
    IMPORTING
      e_exit     = lc_exit
      es_variant = e_def_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid
            TYPE 'S' NUMBER sy-msgno
            DISPLAY LIKE 'E'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF lc_exit = space.
      p_vari = e_def_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_ALV_REUSE_VARIANT_F4

*&---------------------------------------------------------------------*
*&      Form  YF_TRANSFERENCIA_ESTOQUE
*&---------------------------------------------------------------------*
FORM yf_transferencia_estoque USING linha CHANGING sf_excbapi.

  DATA: lc_plant           TYPE werks_d,
        lc_charg           TYPE charg_d,
        lc_valor_vincular  TYPE labst,
        lc_valor_vinculada TYPE labst,
        i_bukrs            TYPE t001-bukrs,
        v_charg            TYPE mchb-charg,
        dta_saida          TYPE bapi2017_gm_head_01-pstng_date.

  DATA: it_mchb_aux TYPE TABLE OF mchb INITIAL SIZE 0 WITH HEADER LINE,
        wa_mchb_aux TYPE mchb.

  REFRESH: yt_goodsmvtitm,
           yt_return.

  CLEAR:  yt_goodsmvtitm,
          yt_return,
          sf_excbapi.

  CLEAR: wa_goodsmvtitm.

* Obtem o material selecionado

  IF linha = 0.
    LOOP AT yt_saida INTO wa_saida WHERE selkz = cc_x.
      IF wa_saida-status = icon_generate.
        wa_saida-status = icon_checked.
        MODIFY yt_saida FROM wa_saida INDEX sy-tabix TRANSPORTING status.
        PERFORM yf_goodsmvt_header USING space.
        v_charg = wa_saida-charg.
        IF v_charg GT '2019'.
          CONCATENATE v_charg '_' wa_saida-werks INTO v_charg.
        ENDIF.
*--> 15.06.2023 - Migration S4 – MIGNOW - Start
        "        WA_GOODSMVTITM-MATERIAL     =  WA_SAIDA-MATNR.
        DATA(v_len) = strlen( wa_saida-matnr ).
        IF v_len > 18.
          wa_goodsmvtitm-material_long = wa_saida-matnr .
        ELSE.
          wa_goodsmvtitm-material = wa_saida-matnr .
        ENDIF.
*<-- 15.06.2023 - Migration S4 – MIGNOW – End
        wa_goodsmvtitm-plant        =  wa_saida-werks2. "A Fixar
        wa_goodsmvtitm-stge_loc     =  wa_saida-lgort.
        wa_goodsmvtitm-batch        =  v_charg.  " Lote_FILIAL
        "
        wa_goodsmvtitm-move_type    =  cc_bwart_301.
        wa_goodsmvtitm-move_plant   =  wa_saida-werks.
        wa_goodsmvtitm-move_stloc   =  wa_saida-lgort.
        wa_goodsmvtitm-move_batch   =  wa_saida-charg.
        wa_goodsmvtitm-entry_qnt    =  wa_goodsmvtitm-entry_qnt + ( wa_saida-clabs * -1 ).

        IF NOT ( wa_goodsmvtitm-entry_qnt  IS INITIAL ).
          APPEND wa_goodsmvtitm TO yt_goodsmvtitm.
        ENDIF.

        CLEAR: wa_goodsmvtitm, wa_saida.
      ENDIF.
    ENDLOOP.
  ELSE.
    READ TABLE yt_saida INTO wa_saida INDEX linha.
    IF wa_saida-status = icon_generate.
      wa_saida-status = icon_checked.
      MODIFY yt_saida FROM wa_saida INDEX linha TRANSPORTING status.
      PERFORM yf_goodsmvt_header USING space.
      v_charg = wa_saida-charg.
      IF v_charg GT '2019' AND vg_land1 NE 'AR'.
        CONCATENATE v_charg '_' wa_saida-werks INTO v_charg.
      ENDIF.
*--> 15.06.2023 - Migration S4 – MIGNOW - Start
      "      WA_GOODSMVTITM-MATERIAL   =  WA_SAIDA-MATNR.
      v_len = strlen( wa_saida-matnr ).
      IF v_len > 18.
        wa_goodsmvtitm-material_long = wa_saida-matnr .
      ELSE.
        wa_goodsmvtitm-material = wa_saida-matnr .
      ENDIF.
*<-- 15.06.2023 - Migration S4 – MIGNOW – End








      wa_goodsmvtitm-plant      =  wa_saida-werks2. "A Fixar
      wa_goodsmvtitm-stge_loc   =  wa_saida-lgort.
      wa_goodsmvtitm-batch      =  v_charg. " Lote_FILIAL
      "
      wa_goodsmvtitm-move_type  =  cc_bwart_301.
      wa_goodsmvtitm-move_plant =  wa_saida-werks.
      wa_goodsmvtitm-move_stloc =  wa_saida-lgort.
      wa_goodsmvtitm-move_batch =  wa_saida-charg.
      wa_goodsmvtitm-entry_qnt  =  wa_goodsmvtitm-entry_qnt + ( wa_saida-clabs * -1 ).


      IF NOT ( wa_goodsmvtitm-entry_qnt  IS INITIAL ).
        APPEND wa_goodsmvtitm TO yt_goodsmvtitm.
      ENDIF.

      CLEAR: wa_goodsmvtitm, wa_saida.
    ENDIF.
  ENDIF.



  CHECK NOT yt_goodsmvtitm[] IS INITIAL.

  SELECT bukrs branch
    FROM j_1bbranch
    INTO TABLE it_j_1bbranch
    FOR ALL ENTRIES IN yt_saida
  WHERE branch EQ yt_saida-werks.

  LOOP AT it_j_1bbranch INTO wa_j_1bbranch.

    i_bukrs = wa_j_1bbranch-bukrs.

    CALL FUNCTION 'MR_PERIOD_DETERMINE'
      EXPORTING
        i_bukrs                = i_bukrs
        i_budat                = e_goodsmvthedr-pstng_date
      EXCEPTIONS
        invalid_posting_period = 1
        marv_no_entry          = 2
        OTHERS                 = 3.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    dta_saida = e_goodsmvthedr-pstng_date.

    dta_saida =  dta_saida  + 1.

    CALL FUNCTION 'MR_PERIOD_DETERMINE'
      EXPORTING
        i_bukrs                = i_bukrs
        i_budat                = dta_saida
      EXCEPTIONS
        invalid_posting_period = 1
        marv_no_entry          = 2
        OTHERS                 = 3.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDLOOP.

  sf_excbapi = cc_x.
  PERFORM yf_executa_bapi_goodsmvt.
ENDFORM.                    " YF_TRANSFERENCIA_ESTOQUE

*&---------------------------------------------------------------------*
*&      Form  YF_GOODSMVT_HEADER
*&---------------------------------------------------------------------*
FORM yf_goodsmvt_header USING p_primeiro.

  CLEAR: e_goodsmvthedr,
         e_goodsmvtcode.

  CONCATENATE p_period '01' INTO vd_data_inicial.


  IF  p_primeiro = cc_x.
    CALL FUNCTION 'HR_PSD_DATES_ADD_MONTHS'
      EXPORTING
        v_date       = vd_data_inicial
        v_months     = 1
      IMPORTING
        e_date       = vd_data_inicial
      EXCEPTIONS
        not_positive = 1
        OTHERS       = 2.
    vd_data_inicial+06 = 01.
    e_goodsmvthedr-pstng_date = vd_data_inicial.
    e_goodsmvthedr-doc_date   = vd_data_inicial.
  ELSE.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = vd_data_inicial
      IMPORTING
        last_day_of_month = vd_data_final
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    e_goodsmvthedr-pstng_date = vd_data_final.
    e_goodsmvthedr-doc_date   = vd_data_final.
  ENDIF.
  IF r_es_t IS INITIAL AND vg_land1 NE 'AR'.
    e_goodsmvthedr-header_txt = 'EMPRESTIMO'.
  ENDIF.

ENDFORM.                    " YF_GOODSMVT_HEADER

*&---------------------------------------------------------------------*
*&      Form  YF_EXECUTA_BAPI_GOODSMVT
*&---------------------------------------------------------------------*
FORM yf_executa_bapi_goodsmvt.

  FIELD-SYMBOLS <fs1>.

  DATA: li_tabix         TYPE i,
        ln_num1_4(1)     TYPE n,
        lc_matnr_i       TYPE matnr,
        lc_matnr_r       TYPE matnr,
        matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year,
        materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
        headret          TYPE bapi2017_gm_head_ret,
        pstng_date       TYPE bapi2017_gm_head_01-pstng_date,
        wa_zmmt0071      TYPE zmmt0071.

* Tipo de transação a ser executada
  e_goodsmvtcode-gm_code = cc_mb11_code06.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = e_goodsmvthedr
      goodsmvt_code    = e_goodsmvtcode
    IMPORTING
      materialdocument = materialdocument
      matdocumentyear  = matdocumentyear
    TABLES
      goodsmvt_item    = yt_goodsmvtitm
      return           = yt_return.

  SORT yt_return BY type.

* Efetiva alterações com sucesso
  IF NOT materialdocument IS  INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    pstng_date = e_goodsmvthedr-pstng_date + 1.

    CLEAR: headret-mat_doc.




















    IF r_es_t IS INITIAL AND vg_land1 NE 'AR'. " RJF - 115710 - CS2023000364 Inclusao de regra e botao para estorno
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = materialdocument
          matdocumentyear     = matdocumentyear
          goodsmvt_pstng_date = pstng_date
        IMPORTING
          goodsmvt_headret    = headret
        TABLES
          return              = yt_return.

      IF NOT headret-mat_doc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      LOOP AT yt_return.
        CLEAR: yt_bapierro.
        yt_bapierro-dtlanc = pstng_date.
        yt_bapierro-mensa  = yt_return-message.
        APPEND yt_bapierro.

        IF ( yt_bapierro IS INITIAL ).

        ENDIF.
      ENDLOOP.

    ELSEIF r_es_t IS INITIAL AND vg_land1 EQ 'AR'. " RJF - 115710 - CS2023000364 Inclusao de regra e botao para estorno

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = materialdocument
          matdocumentyear     = matdocumentyear
          goodsmvt_pstng_date = pstng_date
        IMPORTING
          goodsmvt_headret    = headret
        TABLES
          return              = yt_return.

      IF NOT headret-mat_doc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.


      LOOP AT yt_return.
        CLEAR: yt_bapierro.
        yt_bapierro-dtlanc = pstng_date.
        yt_bapierro-mensa  = yt_return-message.
        APPEND yt_bapierro.

        IF ( yt_bapierro IS INITIAL ).

        ENDIF.
      ENDLOOP.

    ENDIF.

    wa_zmmt0071-bukrs      = p_bukrs.
    wa_zmmt0071-mesano     = p_period.
    wa_zmmt0071-matnr      = wa_saida-matnr.
    wa_zmmt0071-werks_a    = wa_saida-werks2.
    wa_zmmt0071-werks_f    = wa_saida-werks.
    wa_zmmt0071-lgort      = wa_saida-lgort.
    wa_zmmt0071-charg      = wa_saida-charg.
    wa_zmmt0071-labst_a    = wa_saida-clabs2.
    wa_zmmt0071-labst_f    = wa_saida-clabs.
    wa_zmmt0071-usnam      = sy-uname.
    wa_zmmt0071-dt_entrada = sy-datum.
    wa_zmmt0071-hr_entrada = sy-uzeit.
    MODIFY zmmt0071 FROM wa_zmmt0071.
    COMMIT WORK.

  ENDIF.

  LOOP AT yt_goodsmvtitm.
    li_tabix = sy-tabix.
    LOOP AT yt_return
      WHERE type EQ cc_e.
      WRITE yt_goodsmvtitm-material TO lc_matnr_i
            USING EDIT MASK '==ALPHA'.
      yt_bapierro-matnr  = yt_goodsmvtitm-material.
      yt_bapierro-dtlanc = e_goodsmvthedr-pstng_date.
      yt_bapierro-werks  = yt_goodsmvtitm-plant.
      yt_bapierro-werks2 = yt_goodsmvtitm-move_plant.
      yt_bapierro-mensa  = yt_return-message.
      APPEND yt_bapierro.
      DELETE yt_goodsmvtitm INDEX li_tabix.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " YF_EXECUTA_BAPI_GOODSMVT

*&---------------------------------------------------------------------*
*&      Form  YF_SELECIONAR_MESES
*&---------------------------------------------------------------------*
FORM yf_selecionar_mesano  CHANGING pn_period TYPE kmonth.

  DATA: li_coluna TYPE sy-scols  VALUE 31,
        li_linha  TYPE sy-srows  VALUE 2.

  CLEAR pn_period.

* Exibe tela de seleção para período
  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      actual_month               = sy-datum(6)
      language                   = sy-langu
      start_column               = li_coluna
      start_row                  = li_linha
    IMPORTING
      selected_month             = pn_period
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      OTHERS                     = 4.

  IF sy-subrc IS INITIAL.
    IF pn_period > sy-datum(6) .
      MESSAGE s000 DISPLAY LIKE cc_e WITH TEXT-m01.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_SELECIONAR_MESES

*&---------------------------------------------------------------------*
*&      Form  YF_GERA_RELATORIO_ERRO
*&---------------------------------------------------------------------*
FORM yf_gera_relatorio_erro.

  DATA: lf_new.

  SORT yt_bapierro BY matnr.

* Imprime registros com erros
  LOOP AT yt_bapierro.

    CLEAR lf_new.

*   Se quebrar a linha - imprimir nova linha
    AT NEW matnr.
      IF sy-tabix > 1.
        SKIP 1.
      ENDIF.
      lf_new = cc_x.
      PERFORM yf_muda_cor_da_linha.
    ENDAT.

*   Imprimir nova Linha?
    IF lf_new = cc_x.
      WRITE: /01(18) yt_bapierro-matnr,
              20(10) yt_bapierro-dtlanc,
              31(11) yt_bapierro-werks,
              43(11) yt_bapierro-werks2.
    ENDIF.

*   Imprime mensagem de erro
    IF lf_new = cc_x.
      WRITE 55(78)  yt_bapierro-mensa.
    ELSE.
      WRITE /55(78) yt_bapierro-mensa.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " YF_GERA_RELATORIO_ERRO

*&---------------------------------------------------------------------*
*&      Form  YF_MUDA_COR_DA_LINHA
*&---------------------------------------------------------------------*
FORM yf_muda_cor_da_linha.

  STATICS lf_cor.

  IF  lf_cor  IS  INITIAL.
    FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
    lf_cor = cc_x.
  ELSE.
    FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
    CLEAR  lf_cor.
  ENDIF.

ENDFORM.                    " YF_MUDA_COR_DA_LINHA
*&---------------------------------------------------------------------*
*&      Form  YF_MODIFICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yf_modifica .

  DATA:
    BEGIN OF ltab_values OCCURS 0,
      feld(40) TYPE c,
    END OF ltab_values.

  CLEAR: t001, ltab_fields.
  CLEAR: lc_bukrs, ltab_fields[].

*- *– Set up fields to retrieve data
  ltab_fields-tabname = 'T001'.
  ltab_fields-fieldname = 'BUKRS'.
  ltab_fields-selectflag = 'X'.
  APPEND ltab_fields.

  ltab_fields-tabname = 'T001'.
  ltab_fields-fieldname = 'BUTXT'.
  ltab_fields-selectflag = space.
  APPEND ltab_fields.

  ltab_fields-tabname = 'T001'.
  ltab_fields-fieldname = 'LAND1'.
  ltab_fields-selectflag = space.
  APPEND ltab_fields.

*– Fill values
  SELECT * FROM t001.
    ltab_values-feld = t001-bukrs.
    APPEND ltab_values.
    ltab_values-feld = t001-butxt.
    APPEND ltab_values.
    ltab_values-feld = t001-land1.
    APPEND ltab_values.
  ENDSELECT.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
    EXPORTING
      fieldname                 = 'BUKRS'
      tabname                   = 'T001'
      title_in_values_list      = 'Select a value'
    IMPORTING
      select_value              = lc_bukrs
    TABLES
      fields                    = ltab_fields
      valuetab                  = ltab_values
    EXCEPTIONS
      field_not_in_ddic         = 01
      more_then_one_selectfield = 02
      no_selectfield            = 03.

  IF sy-subrc IS INITIAL.
    p_bukrs = lc_bukrs.

    SELECT SINGLE land1
      INTO vg_land1
      FROM t001
      WHERE bukrs EQ p_bukrs.

  ENDIF.

  IF vg_land1 EQ 'AR'.

    LOOP AT SCREEN.

      IF screen-name EQ 'R_ES_T'.
*        screen-input = 1.
        screen-invisible = 1.
*    ELSE.
*      screen-active = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.


ENDFORM.
