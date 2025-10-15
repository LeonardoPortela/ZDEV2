************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 15.05.2009                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Leitura código de Barras contas de consumo          *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 15.05.2009    Desenvolvedor ABAP   Criação              DEVK905828   *
************************************************************************

REPORT  zimp10.

*-----------------------------------------------------------------------
* Tabelas internas
*-----------------------------------------------------------------------
DATA:

  t_bsik             TYPE TABLE OF bsik,
  w_bsik TYPE bsik,
  t_zimp_contas_cons TYPE TABLE OF zimp_contas_cons.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------
DATA:

  w_zimp_contas_cons TYPE zimp_contas_cons.

*-----------------------------------------------------------------------
* Símbolos de campo
*-----------------------------------------------------------------------
FIELD-SYMBOLS:

  <f_bsik>           TYPE bsik.

*-----------------------------------------------------------------------
* Parâmetros de seleção
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.

PARAMETERS:
  p_emp              TYPE b120-bukrs
                     OBLIGATORY.
SELECT-OPTIONS:
  s_nr_doc           FOR <f_bsik>-belnr.


SELECT-OPTIONS:
  s_forn             FOR <f_bsik>-lifnr
                     NO-EXTENSION NO INTERVALS,

  s_dtlanc           FOR <f_bsik>-budat,
*                     OBLIGATORY.
s_dtvenc             FOR <f_bsik>-budat.
SELECTION-SCREEN END OF BLOCK b01.

*-----------------------------------------------------------------------
* START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Busca dados nas tabelas e verifica a consistência
  PERFORM zf_busca_dados.

* Integra dados de BSIK em ZIMP_CONTAS_CONS
  PERFORM zf_integra_dados.

* Chama tela de atualização
  PERFORM zf_atualizacao.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       Busca dados para processamento
*----------------------------------------------------------------------*
FORM zf_busca_dados .

* Seleciona BSIK
  SELECT *
    INTO TABLE t_bsik
    FROM bsik
    WHERE bukrs  = p_emp    AND
          belnr IN s_nr_doc AND
          budat IN s_dtlanc AND
          zlsch  = 'D'      AND
          shkzg  = 'H'.

  DATA: w_data TYPE sy-datum.

  LOOP AT t_bsik INTO w_bsik.
    w_data = w_bsik-zfbdt + w_bsik-zbd1t.
    IF NOT w_data IN s_dtvenc .
      DELETE t_bsik INDEX sy-tabix.
    ENDIF.
  ENDLOOP.


  IF NOT t_bsik IS INITIAL.

    SORT t_bsik BY belnr.

*   Seleciona documentos já gravados
    SELECT *
      INTO TABLE t_zimp_contas_cons
      FROM zimp_contas_cons
      FOR ALL ENTRIES IN t_bsik
      WHERE bukrs  = p_emp    AND
            belnr  = t_bsik-belnr.

    SORT t_zimp_contas_cons BY belnr.

  ELSE.

*   Verifica se existem registros válidos
    MESSAGE text-e01 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.                    " ZF_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_INTEGRA_DADOS
*&---------------------------------------------------------------------*
*       Integra dados entre as tabelas
*----------------------------------------------------------------------*
FORM zf_integra_dados .

  LOOP AT t_bsik ASSIGNING <f_bsik>.

    READ TABLE t_zimp_contas_cons TRANSPORTING NO FIELDS
      WITH KEY belnr = <f_bsik>-belnr
      BINARY SEARCH.

    IF sy-subrc <> 0.

      MOVE-CORRESPONDING <f_bsik> TO w_zimp_contas_cons.
      INSERT into zimp_contas_cons values w_zimp_contas_cons.

    ENDIF.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " ZF_INTEGRA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZACAO
*&---------------------------------------------------------------------*
*       Atualização da tabela
*----------------------------------------------------------------------*
FORM zf_atualizacao .

  DATA:
    lt_vimsellist TYPE TABLE OF vimsellist,
    lt_vimexclfun TYPE TABLE OF vimexclfun.

  DATA:
    lw_vimsellist TYPE vimsellist,
    lw_vimexclfun TYPE vimexclfun.


  lw_vimexclfun-function = 'NEWL'.
  APPEND lw_vimexclfun TO lt_vimexclfun.

  lw_vimsellist-viewfield = 'BUKRS'.
  lw_vimsellist-operator  = 'EQ'.
  lw_vimsellist-and_or    = 'AND'.
  lw_vimsellist-value     = p_emp.
  APPEND lw_vimsellist TO lt_vimsellist.

  lw_vimsellist-viewfield = 'BELNR'.
  lw_vimsellist-and_or    = 'OR'.

  LOOP AT t_bsik ASSIGNING <f_bsik>.

    lw_vimsellist-value = <f_bsik>-belnr.
    APPEND lw_vimsellist TO lt_vimsellist.

  ENDLOOP.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action              = 'U'
      view_name           = 'ZIMP_CONTAS_CONS'
      check_ddic_mainflag = 'X'
    TABLES
      dba_sellist         = lt_vimsellist
      excl_cua_funct      = lt_vimexclfun
    EXCEPTIONS
      OTHERS              = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ZF_ATUALIZACAO
