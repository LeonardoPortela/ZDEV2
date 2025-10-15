************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 19.06.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório de aprensantação da DRE                   *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 19.06.2008    Michely              Criação              DEVK904304   *
* 31.07.2008    Marcus.Barbara       Alteração            DEVK904591   *
* 19.08.2008    Leonardo Oliveira    Alteração            DEVK904710   *
* 29.09.2008    Marcus.Barbara       Alteração            DEVK904966   *
* 17.10.2008    Marcus.Barbara       Alteração            DEVK905100   *
* 20.10.2008    Marcus.Barbara       Alteração            DEVK905106   *
* 10.12.2008    Marcus.Barbara       Alteração            DEVK905322   *
* 15.12.2008    Marcus.Barbara       Alteração            DEVK905332   *
* 16.12.2008    Marcus.Barbara       Alteração            DEVK905336   *
* 16.12.2008    Marcus.Barbara       Alteração            DEVK905342   *
* 18.12.2008    Marcus.Barbara       Alteração            DEVK905357   *
* 19.12.2008    Marcus.Barbara       Alteração            DEVK905361   *
* 03.03.2009    Marcus.Barbara       Alteração            DEVK905577   *
* 16.03.2009    Marcus.Barbara       Alteração            DEVK905667   *
* 23.03.2009    Marcus.Barbara       Alteração            DEVK905699   *
************************************************************************

REPORT  zgl004_dre_relatorio.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPE-POOLS: stree, klfaz.

*&---------------------------------------------------------------------*
*& Declarações de Tabelas.
*&---------------------------------------------------------------------*
CONSTANTS: c_mark TYPE c VALUE 'X'.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
DATA: BEGIN OF wa_conta,
        saknr            LIKE skat-saknr,
        txt50            LIKE skat-txt50,
      END   OF wa_conta,

      BEGIN OF wa_custo,
        kostl            LIKE cskt-kostl,
        ltext            LIKE cskt-ltext,
      END   OF wa_custo,

      BEGIN OF wa_dre_008_nivel,
        nivel_total      LIKE zgl008_dre_total-nivel,
        nivel            LIKE zgl008_dre_total-nivel,
        sinal(1),
      END OF wa_dre_008_nivel,

      BEGIN OF wa_ordem,
        aufnr            LIKE aufk-aufnr,
        ktext            LIKE aufk-ktext,
      END   OF wa_ordem,

      BEGIN OF wa_lucro,
        prctr            LIKE cepct-prctr,
        ltext            LIKE cepct-ltext,
      END   OF wa_lucro,

      wa_dados           LIKE zst_gl004_dados,
      wa_dados2          LIKE zst_gl004_dados,
      wa_dre_001         LIKE zgl001_dre_est,
      wa_dre_002         LIKE zgl002_dre_est,
      wa_dre_003         LIKE zgl003_dre_est,
      wa_dre_004         LIKE zgl004_dre_est,
      wa_dre_005         LIKE zgl005_dre_dados,
      wa_dre_006         LIKE zgl006_dre_dados,
      wa_dre_007         LIKE zgl007_dre_dados,
      wa_dre_008         LIKE zgl008_dre_total,
      wa_mes             LIKE t247.

DATA: wa_bdcdata         LIKE bdcdata,
      wa_message         LIKE bdcmsgcoll.

DATA: it_conta           LIKE STANDARD TABLE OF wa_conta,
      it_custo           LIKE STANDARD TABLE OF wa_custo,
      it_ordem           LIKE STANDARD TABLE OF wa_ordem,
      it_lucro           LIKE STANDARD TABLE OF wa_lucro,
      it_dados           LIKE STANDARD TABLE OF wa_dados,
      it_dados2          LIKE STANDARD TABLE OF wa_dados,
      it_dre_001         LIKE STANDARD TABLE OF wa_dre_001,
      it_dre_002         LIKE STANDARD TABLE OF wa_dre_002,
      it_dre_003         LIKE STANDARD TABLE OF wa_dre_003,
      it_dre_004         LIKE STANDARD TABLE OF wa_dre_004,
      it_dre_005         LIKE STANDARD TABLE OF wa_dre_005,
      it_dre_006         LIKE STANDARD TABLE OF wa_dre_006,
      it_dre_007         LIKE STANDARD TABLE OF wa_dre_007,
      it_dre_008         LIKE STANDARD TABLE OF wa_dre_008,
      it_dre_008_nivel   LIKE STANDARD TABLE OF wa_dre_008_nivel,
      it_mes             LIKE STANDARD TABLE OF wa_mes,
      it_bdcdata         LIKE STANDARD TABLE OF wa_bdcdata,
      it_message         LIKE STANDARD TABLE OF wa_message.

*----------------------------------------------------------------------*
*  Tabelas Internas (ALV Tree) / Includes
*----------------------------------------------------------------------*

DATA tree1               TYPE REF TO cl_gui_alv_tree.
DATA mr_toolbar          TYPE REF TO cl_gui_toolbar.

INCLUDE <icon>.
INCLUDE bcalv_toolbar_event_receiver.
*include zalv_tree_event_gl004.

DATA toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver.

DATA: it_fieldcatalog   TYPE lvc_t_fcat, "Fieldcatalog.
      it_alvtree        LIKE STANDARD TABLE OF wa_dados,
      go_tbm            TYPE REF TO cl_alv_tree_toolbar_manager,
      ok_code           LIKE sy-ucomm,
      vg_titulo         LIKE zgl001_dre_est-vstxt,
      dataini          LIKE sy-datum,
      datafim          LIKE sy-datum,
      txdataini(8),
      txdatafim(8),
      vlr_acm   LIKE zst_gl004_dados-vlr_acm,
      vlr_rea   LIKE zst_gl004_dados-vlr_rea.

INCLUDE zgl004_include.

*--------------------

DATA: BEGIN OF wa_list OCCURS 50.       " Internal table hierarchy
        INCLUDE STRUCTURE snodetext.
DATA: END OF wa_list.

DATA: it_list            LIKE STANDARD TABLE OF wa_list,
      f15                TYPE c.

*----------------------------------------------------------------------*
* Variávei globais
*----------------------------------------------------------------------*
DATA: vg_chave           TYPE c LENGTH 14,
      vg_fm_name         TYPE rs38l_fnam, "Nome da função smart form
      vg_mes             LIKE t247-ltx,
      vg_empresa         LIKE t001-butxt.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETER:
          r_imp       RADIOBUTTON GROUP tp DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-s04.
PARAMETERS:
      b_nvl    AS CHECKBOX DEFAULT 'X',
      b_cnt    AS CHECKBOX DEFAULT 'X',
      b_obj    AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK b4.
PARAMETERS:
          r_ana       RADIOBUTTON GROUP tp.
SELECTION-SCREEN END   OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s01.
PARAMETERS:
           p_bukrs       LIKE zgl005_dre_dados-bukrs OBLIGATORY,
           p_versn       LIKE zgl005_dre_dados-versn MATCHCODE OBJECT zversn_dre OBLIGATORY,
           p_monat       LIKE zgl005_dre_dados-monat OBLIGATORY,
           p_gjahr       LIKE zgl005_dre_dados-gjahr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b0.

*SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
*PARAMETERS:
*        P_TITULO  LIKE ZGL001_DRE_EST-VSTXT OBLIGATORY.
*SELECTION-SCREEN END   OF BLOCK B3.
*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.
*  SET PF-STATUS 'TELA_1000'.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  DATA: vg_funcao LIKE zgl001_dre_est-funcao.

  SELECT SINGLE funcao INTO vg_funcao
    FROM zgl001_dre_est
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn.

  DATA: BEGIN OF wa_usuario,
      bukrs LIKE zgl009_usuar_dre-bukrs,
    END OF wa_usuario.

  DATA: it_usuario LIKE STANDARD TABLE OF wa_usuario.

  SELECT bukrs
    FROM zgl009_usuar_dre
    INTO TABLE it_usuario
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn
     AND tcode EQ sy-tcode
     AND usnam EQ sy-uname.

  IF sy-subrc EQ 0.
    MESSAGE 'Usuário sem permissão de consulta nesta estrutura de DRE!' TYPE 'E'.
  ENDIF.

  IF vg_funcao NE 'F'.
    IF vg_funcao EQ 'G'.
      MESSAGE 'A Estrutura informada é para DRE Gerencial!' TYPE 'E'.
    ELSEIF vg_funcao IS INITIAL.
      MESSAGE 'Estrutura informada não possue uma função definida! (ZGL002)' TYPE 'E'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR vg_titulo.

  SELECT SINGLE vstxt INTO vg_titulo
    FROM zgl001_dre_est
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn.

  CONCATENATE p_gjahr p_monat '01' INTO dataini.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = dataini
    IMPORTING
      last_day_of_month = datafim.

  CONCATENATE dataini+6(2) dataini+4(2) dataini(4) INTO txdataini.
  CONCATENATE datafim+6(2) datafim+4(2) datafim(4) INTO txdatafim.

* Gero chave conforme parâmetro
  CONCATENATE p_bukrs
              p_versn
              p_monat
              p_gjahr
              INTO vg_chave.
* Busco dados para o relatorio.S
  PERFORM f_busca_dados.
* Monto estrutura de dados para chamada do relatorio
  PERFORM f_monta_dados.
* Se selecionado tipo de apresentação IMPRESSÃO
  IF r_imp IS NOT INITIAL.
* Totaliza niveis e contas sem OBJ de custo
    PERFORM f_totaliza.
* Chamo relatorio Smart Form.
    PERFORM f_chama_relatorio.
  ELSE.
* Chamo ALV tri list
*    perform f_dados_alv.
    CALL SCREEN 100.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busca dados para montar relatorio Smart Form
*----------------------------------------------------------------------*
FORM f_busca_dados.
  SELECT *
    INTO TABLE it_dre_001
    FROM zgl001_dre_est
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn.

  SELECT *
    INTO TABLE it_dre_002
    FROM zgl002_dre_est
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn
   ORDER BY ordnv.

  SELECT *
    FROM zgl003_dre_est
    INTO TABLE it_dre_003
     FOR ALL ENTRIES IN it_dre_002
   WHERE bukrs EQ it_dre_002-bukrs
     AND versn EQ it_dre_002-versn
     AND nivel EQ it_dre_002-nivel.
*   order by saknr.

  SELECT saknr txt50
    FROM skat
    INTO TABLE it_conta
     FOR ALL ENTRIES IN it_dre_003
   WHERE saknr EQ it_dre_003-saknr
     AND ktopl EQ '0050'
     AND spras EQ 'PT'.

  SELECT *
    FROM zgl004_dre_est
    INTO TABLE it_dre_004
     FOR ALL ENTRIES IN it_dre_003
   WHERE bukrs EQ it_dre_003-bukrs
     AND versn EQ it_dre_003-versn
     AND nivel EQ it_dre_003-nivel
     AND saknr EQ it_dre_003-saknr.

  SELECT kostl ltext
    FROM cskt
    INTO TABLE it_custo
     FOR ALL ENTRIES IN it_dre_004
   WHERE kostl EQ it_dre_004-kostl.

  SELECT aufnr ktext
    FROM aufk
    INTO TABLE it_ordem
     FOR ALL ENTRIES IN it_dre_004
   WHERE aufnr EQ it_dre_004-aufnr.

  SELECT prctr ltext
    FROM cepct
    INTO TABLE it_lucro
     FOR ALL ENTRIES IN it_dre_004
   WHERE prctr EQ it_dre_004-prctr.

  SELECT SINGLE *
    FROM zgl005_dre_dados
    INTO wa_dre_005
   WHERE chave EQ vg_chave.

  SELECT *
    FROM zgl006_dre_dados
    INTO TABLE it_dre_006
   WHERE chave EQ vg_chave.

  SELECT *
    FROM zgl007_dre_dados
    INTO TABLE it_dre_007
   WHERE chave EQ vg_chave.
ENDFORM.                    " F_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Monto estrutura de dados para chamada do relatorio
*----------------------------------------------------------------------*
FORM f_monta_dados.
  DATA : texto(80),
        wl_first(1).

  SORT: it_dre_002 BY nivel,
        it_dre_003 BY nivel saknr,
        it_dre_004 BY nivel saknr kostl aufnr prctr,
        it_conta   BY saknr,
        it_ordem   BY aufnr,
        it_lucro   BY prctr,
        it_custo   BY kostl,
        it_dre_006 BY nivel saknr kostl aufnr prctr,
        it_dre_007 BY nivel saknr kostl aufnr prctr.

  SELECT *
    INTO TABLE it_dre_008
    FROM zgl008_dre_total
   WHERE bukrs EQ p_bukrs
     AND versn EQ p_versn.

  LOOP AT it_dre_008 INTO wa_dre_008.
    PERFORM gera_dre_nivel_total USING wa_dre_008-nivel wa_dre_008-equac .
  ENDLOOP.
*---> 05/07/2023 - Migração S4 - DL
  SORT it_dre_001 BY bukrs versn.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE it_dre_001 INTO wa_dre_001 WITH KEY bukrs = p_bukrs
                                                 versn = p_versn
                                                 BINARY SEARCH.

  LOOP AT it_dre_002 INTO wa_dre_002.
    CLEAR: wa_dados.
    wa_dados-nivel  = wa_dre_002-nivel.
    wa_dados-desnvl = wa_dre_002-desnvl.
    wa_dados-ordnv  = wa_dre_002-ordnv.
    wa_dados-tlevel = wa_dre_002-tlevel.
    wa_dados-nvl1   = wa_dre_002-ordnv(2).
    wa_dados-nvl2   = wa_dre_002-ordnv+2(2).
    wa_dados-nvl3   = wa_dre_002-ordnv+4(2).
    wa_dados-nvl4   = wa_dre_002-ordnv+6(2).
    wa_dados-nvl5   = wa_dre_002-ordnv+8(2).
    wa_dados-nvl6   = wa_dre_002-ordnv+10(2).
    wa_dados-nvl7   = wa_dre_002-ordnv+12(2).
    wa_dados-nvl8   = wa_dre_002-ordnv+14(2).
    wa_dados-nvl9   = wa_dre_002-ordnv+16(2).
    wa_dados-nvl10  = wa_dre_002-ordnv+18(2).


    READ TABLE it_dre_003 INTO wa_dre_003 WITH KEY nivel = wa_dre_002-nivel
                                                   BINARY SEARCH.
    IF sy-subrc EQ 0. "Caso o tenha uma conta ligada a este nivel
      APPEND wa_dados TO it_dados.
      CLEAR: wa_dados.
      wa_dados-nivel  = wa_dre_002-nivel.
      wa_dados-desnvl = wa_dre_002-desnvl.
      wa_dados-ordnv  = wa_dre_002-ordnv.
      wa_dados-tlevel = wa_dre_002-tlevel.
      wa_dados-nvl1   = wa_dre_002-ordnv(2).
      wa_dados-nvl2   = wa_dre_002-ordnv+2(2).
      wa_dados-nvl3   = wa_dre_002-ordnv+4(2).
      wa_dados-nvl4   = wa_dre_002-ordnv+6(2).
      wa_dados-nvl5   = wa_dre_002-ordnv+8(2).
      wa_dados-nvl6   = wa_dre_002-ordnv+10(2).
      wa_dados-nvl7   = wa_dre_002-ordnv+12(2).
      wa_dados-nvl8   = wa_dre_002-ordnv+14(2).
      wa_dados-nvl9   = wa_dre_002-ordnv+16(2).
      wa_dados-nvl10  = wa_dre_002-ordnv+18(2).
      wl_first = 'X'.
      LOOP AT it_dre_003 INTO wa_dre_003 WHERE nivel = wa_dre_002-nivel.
        wa_dados-saknr = wa_dre_003-saknr.
        READ TABLE it_conta INTO wa_conta WITH KEY saknr = wa_dre_003-saknr
                                                   BINARY SEARCH.
        wa_dados-txt50 = wa_conta-txt50.
        READ TABLE it_dre_004 INTO wa_dre_004 WITH KEY nivel = wa_dre_003-nivel
                                                       saknr = wa_dre_003-saknr
                                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          CLEAR: wa_dados-vlr_rea, wa_dados-vlr_acm, wa_dados-qtd_ton,
                 wa_dados-ana_acm, wa_dados-qtd_acm, wa_dados-ana_vlr.
          APPEND wa_dados TO it_dados.
          CLEAR wa_dados.
          wa_dados-nivel  = wa_dre_002-nivel.
          wa_dados-desnvl = wa_dre_002-desnvl.
          wa_dados-ordnv  = wa_dre_002-ordnv.
          wa_dados-tlevel  = wa_dre_002-tlevel + 2.
          wa_dados-nvl1   = wa_dre_002-ordnv(2).
          wa_dados-nvl2   = wa_dre_002-ordnv+2(2).
          wa_dados-nvl3   = wa_dre_002-ordnv+4(2).
          wa_dados-nvl4   = wa_dre_002-ordnv+6(2).
          wa_dados-nvl5   = wa_dre_002-ordnv+8(2).
          wa_dados-nvl6   = wa_dre_002-ordnv+10(2).
          wa_dados-nvl7   = wa_dre_002-ordnv+12(2).
          wa_dados-nvl8   = wa_dre_002-ordnv+14(2).
          wa_dados-nvl9   = wa_dre_002-ordnv+16(2).
          wa_dados-nvl10  = wa_dre_002-ordnv+18(2).

          READ TABLE it_conta INTO wa_conta WITH KEY saknr = wa_dre_003-saknr
                                                     BINARY SEARCH.
          wa_dados-txt50 = wa_conta-txt50.
          wa_dados-saknr = wa_dre_003-saknr.
          LOOP AT it_dre_004 INTO wa_dre_004 WHERE nivel = wa_dre_003-nivel
                                               AND saknr = wa_dre_003-saknr.

            CLEAR: wa_dados-vlr_rea, wa_dados-vlr_acm, wa_dados-qtd_ton,
                   wa_dados-ana_acm, wa_dados-qtd_acm, wa_dados-ana_vlr.

            IF wa_dre_004-kostl IS NOT INITIAL. "Centro de Custo
              wa_dados-kostl = wa_dre_004-kostl.
              READ TABLE it_custo INTO wa_custo WITH KEY kostl = wa_dre_004-kostl
                                                         BINARY SEARCH.

              CONCATENATE wa_custo-kostl '-' wa_custo-ltext INTO texto.
              wa_dados-ltext = texto(40).

              CLEAR wa_dre_006.
              READ TABLE it_dre_006 INTO wa_dre_006 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             kostl = wa_dre_004-kostl
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_ton = wa_dre_006-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS
                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR wa_dre_007.
              READ TABLE it_dre_007 INTO wa_dre_007 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             kostl = wa_dre_004-kostl
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_acm = wa_dre_007-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-vlr_acm NE 0 ).
                APPEND wa_dados TO it_dados.
              ENDIF.
            ELSEIF wa_dre_004-aufnr IS NOT INITIAL. "Ordem interna
              wa_dados-aufnr = wa_dre_004-aufnr.
              READ TABLE it_ordem INTO wa_ordem WITH KEY aufnr = wa_dre_004-aufnr
                                                         BINARY SEARCH.
              CONCATENATE wa_ordem-aufnr '-' wa_ordem-ktext INTO texto.
              wa_dados-ltext = texto(40).
              CLEAR wa_dre_006.
              READ TABLE it_dre_006 INTO wa_dre_006 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             aufnr = wa_dre_004-aufnr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_ton = wa_dre_006-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR wa_dre_007.
              READ TABLE it_dre_007 INTO wa_dre_007 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             aufnr = wa_dre_004-aufnr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_acm = wa_dre_007-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*              wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-vlr_acm NE 0 ).
                APPEND wa_dados TO it_dados.
              ENDIF.
            ELSEIF wa_dre_004-prctr IS NOT INITIAL. "Centro de Lucro
              wa_dados-prctr = wa_dre_004-prctr.
              READ TABLE it_lucro INTO wa_lucro WITH KEY prctr = wa_dre_004-prctr
                                                         BINARY SEARCH.
              CONCATENATE wa_lucro-prctr '-' wa_lucro-ltext INTO texto.
              wa_dados-ltext = texto(40).
              CLEAR wa_dre_006.
              READ TABLE it_dre_006 INTO wa_dre_006 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             prctr = wa_dre_004-prctr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_ton = wa_dre_006-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR wa_dre_007.
              READ TABLE it_dre_007 INTO wa_dre_007 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             prctr = wa_dre_004-prctr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_acm = wa_dre_007-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-vlr_acm NE 0 ).
                APPEND wa_dados TO it_dados.
              ENDIF.
            ELSE.
              READ TABLE it_dre_006 INTO wa_dre_006 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_ton = wa_dre_006-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_006-qtd_ton GT 0.
                    wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              CLEAR wa_dre_007.
              READ TABLE it_dre_007 INTO wa_dre_007 WITH KEY nivel = wa_dre_004-nivel
                                                             saknr = wa_dre_004-saknr
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_dados-qtd_acm = wa_dre_007-qtd_ton.
                IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS
                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

                  IF wa_dre_007-qtd_ton GT 0.
                    wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-vlr_acm NE 0 ).
                MOVE wa_conta-txt50 TO wa_dados-ltext.
                "concatenate 'c. contabil:' wa_dados-saknr into wa_dados-ltext SEPARATED BY space.
                APPEND wa_dados TO it_dados.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          CLEAR: wa_dados-vlr_rea, wa_dados-vlr_acm, wa_dados-qtd_ton,
                 wa_dados-ana_acm, wa_dados-qtd_acm, wa_dados-ana_vlr.

          CLEAR wa_dre_006.
          READ TABLE it_dre_006 INTO wa_dre_006 WITH KEY nivel = wa_dre_003-nivel
                                                         saknr = wa_dre_003-saknr
                                                         BINARY SEARCH.
          IF sy-subrc = 0.

            wa_dados-qtd_ton = wa_dre_006-qtd_ton.
            IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

              IF wa_dre_006-qtd_ton GT 0.
                wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
              ENDIF.
            ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_rea = wa_dre_006-vlr_rea.
               wa_dados-vlr_rea = CONV #( wa_dre_006-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

              IF wa_dre_006-qtd_ton GT 0.
                wa_dados-ana_vlr = wa_dre_006-vlr_rea / wa_dre_006-qtd_ton.
              ENDIF.
            ENDIF.


          ENDIF.
          CLEAR wa_dre_007.
          READ TABLE it_dre_007 INTO wa_dre_007 WITH KEY nivel = wa_dre_003-nivel
                                                         saknr = wa_dre_003-saknr
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_dados-qtd_acm = wa_dre_007-qtd_ton.
            IF wa_dre_001-waers EQ 'USD'.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

              IF wa_dre_007-qtd_ton GT 0.
                wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
              ENDIF.
            ELSE.
*---> 09/06/2023 - Migração S4 - JS
*               wa_dados-vlr_acm = wa_dre_007-vlr_rea.
               wa_dados-vlr_acm = CONV #( wa_dre_007-vlr_rea ).
*<--- 09/06/2023 - Migração S4 - JS

              IF wa_dre_007-qtd_ton GT 0.
                wa_dados-ana_acm = wa_dre_007-vlr_rea / wa_dre_007-qtd_ton.
              ENDIF.
            ENDIF.
          ENDIF.
          IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-vlr_acm NE 0 ).
            "concatenate 'c. contabil:' wa_dados-saknr into wa_dados-ltext SEPARATED BY space.
            MOVE wa_conta-txt50 TO wa_dados-ltext.
            APPEND wa_dados TO it_dados.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Caso o tenha uma conta ligada a este nivel
      APPEND wa_dados TO it_dados.
    ENDIF.

  ENDLOOP.

  DATA: vg_nivel(1).


  LOOP AT it_dre_002 INTO wa_dre_002.

    CLEAR: vlr_acm,
           vlr_rea,
           it_dados2,
           vg_nivel.

    LOOP AT it_dre_008_nivel INTO wa_dre_008_nivel WHERE nivel_total EQ wa_dre_002-nivel.
      LOOP AT it_dados INTO wa_dados2.
        PERFORM verifica_nivel USING wa_dre_008_nivel-nivel wa_dados2-nivel vg_nivel.
        IF vg_nivel EQ 'X'.
          IF wa_dre_008_nivel-sinal EQ '-'.
            vlr_acm = vlr_acm - wa_dados2-vlr_acm.
            vlr_rea = vlr_rea - wa_dados2-vlr_rea.
          ELSEIF wa_dre_008_nivel-sinal EQ '+'.
            vlr_acm = vlr_acm + wa_dados2-vlr_acm.
            vlr_rea = vlr_rea + wa_dados2-vlr_rea.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF ( vlr_acm IS NOT INITIAL ) OR ( vlr_rea IS NOT INITIAL ).
      READ TABLE it_dados INTO wa_dados WITH KEY nivel = wa_dre_002-nivel.
      IF sy-subrc EQ 0.
        wa_dados-vlr_acm = vlr_acm.
        wa_dados-vlr_rea = vlr_rea.
        MODIFY it_dados INDEX sy-tabix
          FROM wa_dados TRANSPORTING vlr_rea vlr_acm.
      ENDIF.
    ENDIF.

  ENDLOOP.

*  LOOP AT IT_DADOS INTO WA_DADOS2.
*
*    CLEAR: VLR_ACM, VLR_REA.
*
*    LOOP AT IT_DADOS INTO WA_DADOS.
*      PERFORM VERIFICA_NIVEL USING WA_DADOS2-NIVEL WA_DADOS-NIVEL VG_NIVEL.
*      IF VG_NIVEL EQ 'X'.
*        VLR_ACM = VLR_ACM + WA_DADOS-VLR_ACM.
*        VLR_REA = VLR_REA + WA_DADOS-VLR_REA.
*      ENDIF.
*    ENDLOOP.
*
*    IF ( VLR_ACM IS NOT INITIAL ) OR ( VLR_REA IS NOT INITIAL ).
*      READ TABLE IT_DADOS INTO WA_DADOS WITH KEY NIVEL = WA_DADOS2-NIVEL.
*      IF SY-SUBRC EQ 0.
*        WA_DADOS-VLR_ACM = VLR_ACM.
*        WA_DADOS-VLR_REA = VLR_REA.
*        MODIFY IT_DADOS INDEX SY-TABIX
*          FROM WA_DADOS TRANSPORTING VLR_REA VLR_ACM.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.                    " F_MONTA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_CHAMA_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_chama_relatorio .
  DATA: vl_mes           LIKE t247-ltx,
        vl_empresa       LIKE t001-butxt.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = 'ZGL004_DRE_FORM'
    IMPORTING
      fm_name  = vg_fm_name
    EXCEPTIONS
      OTHERS   = 3.

  CHECK sy-subrc IS INITIAL.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = it_mes.

  SORT: it_mes   BY mnr,
        it_dados BY ordnv txt50 ltext.

  READ TABLE it_mes INTO wa_mes WITH KEY mnr = p_monat.

  SELECT SINGLE butxt
    FROM t001
    INTO vl_empresa
   WHERE bukrs EQ p_bukrs.

  CALL FUNCTION vg_fm_name
    EXPORTING
      mes     = wa_mes-ltx
      ano     = p_gjahr
      empresa = vl_empresa
      moeda   = wa_dre_001-waers
      usuario = wa_dre_005-uname
      data    = wa_dre_005-datum
      hora    = wa_dre_005-uzeit
      titulo  = vg_titulo
      nivel   = b_nvl
      conta   = b_cnt
      objeto  = b_obj
    TABLES
      dados   = it_dados.


ENDFORM.                    " F_CHAMA_RELATORIO
*&---------------------------------------------------------------------*
*&      Form  F_TOTALIZA
*&---------------------------------------------------------------------*
*       Totalizar niveis e contas com dependentes
*----------------------------------------------------------------------*
FORM f_totaliza .
  DATA: vl_ord             LIKE zgl002_dre_est-ordnv,
        vl_idx             LIKE sy-tabix.
  SORT: it_dados BY ordnv txt50 ltext.
  CLEAR vl_idx.

  LOOP AT it_dados INTO wa_dados.
    IF wa_dados-saknr IS INITIAL.
      vl_idx = sy-tabix.
      CONCATENATE wa_dados-ordnv
                  '%' INTO vl_ord.
      SELECT *
        FROM zgl002_dre_est
        INTO TABLE it_dre_002
       WHERE bukrs EQ p_bukrs
         AND versn EQ p_versn
         AND ordnv LIKE vl_ord.

*      IF WA_DADOS-SAKNR IS INITIAL.
      SELECT *
        FROM zgl006_dre_dados
        INTO TABLE it_dre_006
         FOR ALL ENTRIES IN it_dre_002
       WHERE chave EQ vg_chave
         AND nivel EQ it_dre_002-nivel.

      SELECT *
        FROM zgl007_dre_dados
        INTO TABLE it_dre_007
         FOR ALL ENTRIES IN it_dre_002
       WHERE chave EQ vg_chave
         AND nivel EQ it_dre_002-nivel.
*      ELSE.
*        SELECT *
*          FROM ZGL006_DRE_DADOS
*          INTO TABLE IT_DRE_006
*           FOR ALL ENTRIES IN IT_DRE_002
*         WHERE CHAVE EQ VG_CHAVE
*           AND NIVEL EQ IT_DRE_002-NIVEL
*           AND SAKNR EQ WA_DADOS-SAKNR.
*
*        SELECT *
*          FROM ZGL007_DRE_DADOS
*          INTO TABLE IT_DRE_007
*           FOR ALL ENTRIES IN IT_DRE_002
*         WHERE CHAVE EQ VG_CHAVE
*           AND NIVEL EQ IT_DRE_002-NIVEL
*           AND SAKNR EQ WA_DADOS-SAKNR.
*      ENDIF.

      LOOP AT it_dre_006 INTO wa_dre_006.
        IF wa_dre_001-waers EQ 'BRL'.
          wa_dados-vlr_rea = wa_dados-vlr_rea + wa_dre_006-vlr_rea.
        ELSE.
          wa_dados-vlr_rea = wa_dados-vlr_rea + wa_dre_006-vlr_rea_cnv.
        ENDIF.
        wa_dados-qtd_ton = wa_dados-qtd_ton + wa_dre_006-qtd_ton.
      ENDLOOP.
      IF wa_dados-qtd_ton GT 0.
        wa_dados-ana_vlr = wa_dados-vlr_rea / wa_dados-qtd_ton.
      ENDIF.

      LOOP AT it_dre_007 INTO wa_dre_007.
        IF wa_dre_001-waers EQ 'BRL'.
          wa_dados-vlr_acm = wa_dados-vlr_acm + wa_dre_007-vlr_rea.
        ELSE.
          wa_dados-vlr_acm = wa_dados-vlr_acm + wa_dre_007-vlr_rea_cnv.
        ENDIF.
        wa_dados-qtd_acm = wa_dados-qtd_ton + wa_dre_007-qtd_ton.
      ENDLOOP.
      IF wa_dados-qtd_acm GT 0.
        wa_dados-ana_acm = wa_dados-vlr_acm / wa_dados-qtd_acm.
      ENDIF.
      MODIFY it_dados INDEX vl_idx FROM wa_dados TRANSPORTING vlr_rea qtd_ton vlr_acm qtd_acm.
    ENDIF.
  ENDLOOP.

  CLEAR: it_dados2.
  LOOP AT it_dados INTO wa_dados.

    IF ( NOT wa_dados-saknr IS INITIAL ).
      IF ( wa_dados-vlr_rea NE 0 ) OR ( wa_dados-qtd_ton NE 0 )
      OR ( wa_dados-vlr_acm NE 0 ) OR ( wa_dados-qtd_acm NE 0 ).
        APPEND wa_dados TO it_dados2.
      ENDIF.
    ELSE.
      APPEND wa_dados TO it_dados2.
    ENDIF.
  ENDLOOP.

  CLEAR: it_dados.

  it_dados = it_dados2.

ENDFORM.                    " F_TOTALIZA

*&---------------------------------------------------------------------*
*&      Form  GERA_DRE_NIVEL_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_dre_nivel_total  USING  p_wa_dre_008_nivel p_wa_dre_008_equac.

  DATA: pc TYPE i,
        px TYPE i,
        pm TYPE i,
        tc TYPE c,
        sn TYPE c,
        nv(30),
        eq LIKE zgl008_dre_total-equac.

  pc = 1.
  sn = 'N'.
  eq = p_wa_dre_008_equac.
  CLEAR: nv.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      string = eq
    IMPORTING
      length = pm.

  WHILE pc LE pm.

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = eq
      IMPORTING
        length = px.

    IF px NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          string  = eq
          pos     = 1
        IMPORTING
          string1 = tc
          string2 = eq.
    ELSE.
      tc = eq.
    ENDIF.

    IF tc IS NOT INITIAL.

      wa_dre_008_nivel-nivel_total = wa_dre_008-nivel.

      IF ( tc EQ '-' ) OR ( tc EQ '+' ).
        IF sn = 'S'.
          wa_dre_008_nivel-nivel = nv.
          CLEAR: nv.
          APPEND wa_dre_008_nivel TO it_dre_008_nivel.
        ENDIF.

        CLEAR: wa_dre_008_nivel.
        wa_dre_008_nivel-sinal = tc.
        sn = 'S'.
      ELSE.
        IF nv IS NOT INITIAL.
          CALL FUNCTION 'STRING_CONCATENATE'
            EXPORTING
              string1 = nv
              string2 = tc
            IMPORTING
              string  = nv.
        ELSE.
          nv = tc.
        ENDIF.
        IF pc EQ pm.
          wa_dre_008_nivel-nivel = nv.
          CLEAR: nv.
          APPEND wa_dre_008_nivel TO it_dre_008_nivel.
        ENDIF.
      ENDIF.
    ENDIF.
    pc = pc + 1.
  ENDWHILE.

ENDFORM.                    " GERA_DRE_NIVEL_TOTAL

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_nivel  USING    p_wa_dre_008_nivel_nivel
                              p_wa_dados2_nivel
                              p_vg_nivel.
  DATA: pc  TYPE i,
        px  TYPE i,
        tc  TYPE c,
        vg1 TYPE c,
        vg2 TYPE c,
        nv1(30),
        nv1_px TYPE i,
        nv2(30),
        nv2_px TYPE i,
        nv_text(30),
        w_wa_dre_008_nivel_nivel(30),
        w_wa_dados2_nivel(30).

  CLEAR: p_vg_nivel,
         nv1,
         nv2.

  w_wa_dre_008_nivel_nivel = p_wa_dre_008_nivel_nivel.
  pc = 1.
  WHILE ( pc LE 30 ) AND ( w_wa_dre_008_nivel_nivel IS NOT INITIAL ).

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = w_wa_dre_008_nivel_nivel
      IMPORTING
        length = px.

    IF px NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          string  = w_wa_dre_008_nivel_nivel
          pos     = 1
        IMPORTING
          string1 = tc
          string2 = w_wa_dre_008_nivel_nivel.
    ELSE.
      tc = w_wa_dre_008_nivel_nivel.
      CLEAR: w_wa_dre_008_nivel_nivel.
    ENDIF.

    IF ( tc NE '0' ) OR ( nv1 IS NOT INITIAL ).
      IF nv1 IS NOT INITIAL.
        CALL FUNCTION 'STRING_CONCATENATE'
          EXPORTING
            string1 = nv1
            string2 = tc
          IMPORTING
            string  = nv1.
      ELSE.
        nv1 = tc.
      ENDIF.
    ENDIF.
    pc = pc + 1.

  ENDWHILE.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      string = nv1
    IMPORTING
      length = nv1_px.

  w_wa_dados2_nivel = p_wa_dados2_nivel.
  pc = 1.

  WHILE ( pc LE 30 ) AND ( p_vg_nivel IS INITIAL ) AND ( w_wa_dados2_nivel IS NOT INITIAL ).

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = w_wa_dados2_nivel
      IMPORTING
        length = px.

    IF px NE 1.
      CALL FUNCTION 'STRING_SPLIT_AT_POSITION'
        EXPORTING
          string  = w_wa_dados2_nivel
          pos     = 1
        IMPORTING
          string1 = tc
          string2 = w_wa_dados2_nivel.
    ELSE.
      tc = w_wa_dados2_nivel.
      CLEAR: w_wa_dados2_nivel.
    ENDIF.

    IF ( tc NE '0' ) OR ( nv2 IS NOT INITIAL ).
      IF nv2 IS NOT INITIAL.
        CALL FUNCTION 'STRING_CONCATENATE'
          EXPORTING
            string1 = nv2
            string2 = tc
          IMPORTING
            string  = nv2.
      ELSE.
        nv2 = tc.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = nv2
      IMPORTING
        length = nv2_px.

    CONCATENATE nv2 w_wa_dados2_nivel INTO nv_text.

    IF ( nv2_px EQ nv1_px ).
      PERFORM par_unit USING nv1 nv_text vg1 vg2.
      IF ( vg1 EQ vg2 ).
        IF ( nv2 EQ nv1 ).
          p_vg_nivel = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    pc = pc + 1.

  ENDWHILE.

ENDFORM.                    " VERIFICA_NIVEL

*&---------------------------------------------------------------------*
*&      Form  PAR_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NV1      --> Nivel Primário
*      -->P_NV_TEXT  --> Nivel Secundário concatenado com o restante não lido
*      -->P_VG1      --> Paridade Primária
*      -->P_VG2      --> Paridade Secundária
*----------------------------------------------------------------------*
FORM par_unit  USING    p_nv1
                        p_nv_text
                        p_vg1
                        p_vg2.

  DATA: q1 TYPE i,
        q2 TYPE i,
        r1 TYPE i,
        r2 TYPE f.

  q1 = 0.
  q2 = 0.

  CLEAR: p_vg1,
         p_vg2.

  IF p_nv1 IS NOT INITIAL.
    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = p_nv1
      IMPORTING
        length = q1.
  ELSE.
    p_vg1 = 'X'.
  ENDIF.

  IF p_nv1 IS NOT INITIAL.
    CALL FUNCTION 'STRING_LENGTH'
      EXPORTING
        string = p_nv_text
      IMPORTING
        length = q2.
  ELSE.
    p_vg2 = 'X'.
  ENDIF.

  IF ( p_vg1 IS INITIAL ) AND ( p_vg2 IS INITIAL ).

    r1 = q1 / 2.
    r2 = q1 / 2.

    IF r1 EQ r2.
      p_vg1 = 'X'.
    ELSE.
      CLEAR p_vg1.
    ENDIF.

    r1 = q2 / 2.
    r2 = q2 / 2.

    IF r1 EQ r2.
      p_vg2 = 'X'.
    ELSE.
      CLEAR p_vg2.
    ENDIF.

  ENDIF.

ENDFORM.                    " PAR_UNIT
