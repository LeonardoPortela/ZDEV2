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
REPORT zles_contingencia_frete_aqua.

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
  PARAMETERS: p_bukrs LIKE zlest0056-bukrs,
              p_werks LIKE zlest0056-werks,
              p_viage LIKE zlest0056-nr_viagem,
              p_ano   LIKE zlest0056-ano_viagem.
  SELECT-OPTIONS: p_data   FOR sy-datum   NO-EXTENSION OBLIGATORY.
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
      IF p_bukrs IS INITIAL AND
         p_werks IS INITIAL AND
         p_viage IS INITIAL AND
         p_ano   IS INITIAL.
        l_operacao = '03'.
      ELSE.
        l_operacao = '04'.
      ENDIF.

    WHEN p_opt2.
      IF p_bukrs IS INITIAL AND
         p_werks IS INITIAL AND
         p_viage IS INITIAL AND
         p_ano   IS INITIAL.
        l_operacao = '01'.
      ELSE.
        l_operacao = '02'.
      ENDIF.
  ENDCASE.

  CALL FUNCTION 'ZLES_BUSCA_DADOS_FRETE_AQUAV'
    EXPORTING
      i_operacao   = l_operacao
      i_data_ini   = p_data-low
      i_data_fim   = p_data-high
      i_bukrs      = p_bukrs
      i_werks      = p_werks
      i_nr_viagem  = p_viage
      i_ano_viagem = p_ano.

  MESSAGE s024(sd) WITH 'Processo de Contingência Finalizado. '
                        'Verifique tabela ZLEST_FRETE_CONT com resultado.'.

ENDFORM.

**********************************************************************
**********************************************************************
