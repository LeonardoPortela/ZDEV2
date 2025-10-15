************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 05.05.2009                                          *
* Tipo de prg ...: Pool de módulos                                     *
* Objetivo    ...: Lançamento/Modificação/Exibição de Tributos a Pagar *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 05.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************

PROGRAM  sapmzimp01.

*-----------------------------------------------------------------------
* Tipos
*-----------------------------------------------------------------------
TYPES:
  BEGIN OF ty_tc_detalhe.
INCLUDE TYPE zimp_detalhe.
TYPES:
  modif              TYPE c,
  mark               TYPE c,
  END OF ty_tc_detalhe.
*-----------------------------------------------------------------------
* Variáveis
*-----------------------------------------------------------------------
DATA:
  v_okcode           TYPE sy-ucomm,
  v_titulo(20)       TYPE c,
  v_total            TYPE zimp_detalhe-vlr_principal,
  v_total_multa      TYPE zimp_detalhe-vlr_multa,
  v_total_juros      TYPE zimp_detalhe-vlr_juros,
  v_desc_lifnr       TYPE lfa1-name1,
  v_cab_txt_cta_forn TYPE lfa1-name1,
  v_cab_txt_cta_imp  TYPE skat-txt50,
  v_cab_txt_cta_jur  TYPE skat-txt50,
  v_cab_txt_cta_mul  TYPE skat-txt50,
* Início Alteração Ricardo Furst 18.07.2009
  v_cab_txt_cta_mon  TYPE skat-txt50,
  v_cab_txt_cta_tse  TYPE skat-txt50,
* Fim Alteração Ricardo Furst 18.07.2009
  v_cab_txt_tip_imp  TYPE zimp_tipos_impos-arrecadacao,
  v_imp_cod_barras   TYPE zimp_tipos_impos-cod_barras,
  v_imp_dt_apuracao  TYPE zimp_tipos_impos-dt_apuracao,
  v_imp_mes_apuracao TYPE zimp_tipos_impos-mes_apuracao,
  v_ktopl            TYPE t001-ktopl.

*-----------------------------------------------------------------------
* Controles de tela
*-----------------------------------------------------------------------
CONTROLS:
  tc_detalhes        TYPE TABLEVIEW USING SCREEN '0100'.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:
  w_zimp_cabecalho   TYPE zimp_cabecalho,
  w_zimp_detalhe     TYPE ty_tc_detalhe,
  w_cols             LIKE LINE OF tc_detalhes-cols.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:
  t_tc_detalhes      TYPE TABLE OF ty_tc_detalhe,
  t_det_aux          TYPE TABLE OF ty_tc_detalhe.

*-----------------------------------------------------------------------
* Constantes
*-----------------------------------------------------------------------
CONSTANTS:
  c_tcode_criar      TYPE sy-tcode VALUE 'ZIMP01',
  c_tcode_modificar  TYPE sy-tcode VALUE 'ZIMP02',
  c_tcode_exibir     TYPE sy-tcode VALUE 'ZIMP03',
  c_0                TYPE c        VALUE '0',
  c_1                TYPE c        VALUE '1',
  c_e                TYPE c        VALUE 'E',
  c_x                TYPE c        VALUE 'X'.


*-----------------------------------------------------------------------
* LOAD-OF-PROGRAM
*-----------------------------------------------------------------------
LOAD-OF-PROGRAM.

  CASE sy-tcode.

    WHEN c_tcode_modificar.
      v_titulo = 'Modificação de'(001).

    WHEN c_tcode_exibir.
      v_titulo = 'Exibição de'(002).

  ENDCASE.
