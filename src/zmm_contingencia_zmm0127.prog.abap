*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 18.01.2022                                              *
* Descrição: Gerenciamento contingencia algodao                      *
* Report   : ZLES_CONTINGENCIA_FRETE_AQUA
*--------------------------------------------------------------------*
* Projeto  : CS2023000189
*--------------------------------------------------------------------*
REPORT zmm_contingencia_zmm0127.

TABLES: zlest0056.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: w_layout   TYPE lvc_s_layo,
      w_stable   TYPE lvc_s_stbl,
      t_alv      TYPE TABLE OF zmmt0008,
      w_alv      TYPE zmmt0008,
      l_tabix    TYPE sy-tabix,
      l_operacao TYPE char02,
      l_resp     TYPE char01,
      l_erro     TYPE char01.

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:     p_opt1  RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND us1.
  PARAMETERS:     p_opt2  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: p_data   FOR sy-datum   NO-EXTENSION OBLIGATORY.
  PARAMETERS p_docnum TYPE j_1bdocnum.
SELECTION-SCREEN END   OF BLOCK b2.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF p_data-low IS INITIAL OR p_data-high IS INITIAL.
    MESSAGE s024(sd) WITH 'Informar a Data para Pesquisa' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

  PERFORM f_processa_dados.

**********************************************************************
* processa
**********************************************************************
FORM f_processa_dados.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Deseja Prosseguir ?'
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = l_resp
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_resp = '1'.

  CASE abap_true.
    WHEN p_opt1.
      l_operacao = '01'.
    WHEN p_opt2.
      l_operacao = '02'.
  ENDCASE.

  CALL FUNCTION 'ZMM_BUSCA_DADOS_NOTAS_ECC'
    EXPORTING
      i_operacao = l_operacao
      i_data_ini = p_data-low
      i_data_fim = p_data-high
      i_docnum   = p_docnum.

  MESSAGE s024(sd) WITH 'Processo de Contingência Finalizado. '.

ENDFORM.
