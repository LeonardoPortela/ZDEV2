*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 18.01.2022                                              *
* Descrição: Gerenciamento contingencia algodao                      *
* Report   : ZSD_CONTINGENCIA_ALGODAO                                *
*--------------------------------------------------------------------*
* Projeto  : CS2023000189
*--------------------------------------------------------------------*
REPORT zsd_contingencia_algodao.

TABLES: zmmt0008.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: w_layout TYPE lvc_s_layo,
      w_stable TYPE lvc_s_stbl,
      t_alv    TYPE TABLE OF zmmt0008,
      w_alv    TYPE zmmt0008,
      l_tabix  TYPE sy-tabix,
      l_resp   TYPE char01,
      l_erro   TYPE char01.

*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS:   p_opt5   RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND us1.
  PARAMETERS:   p_opt1   RADIOBUTTON GROUP g1.
  PARAMETERS:   p_opt2   RADIOBUTTON GROUP g1.
  PARAMETERS:   p_opt6   RADIOBUTTON GROUP g1.
  PARAMETERS:   p_opt3   RADIOBUTTON GROUP g1.
  PARAMETERS:   p_opt4   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_werks LIKE zmmt0008-werks MODIF ID gr1,
              p_lgort LIKE zmmt0008-lgort        MODIF ID gr1,
              p_charg LIKE zmmt0008-charg        MODIF ID gr1,
              p_roman LIKE zmmt0008-nr_romaneio  MODIF ID gr1.
SELECTION-SCREEN END   OF BLOCK b2.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      IF p_opt2 = abap_true.
        screen-active = '1'.
      ELSE.
        screen-active = '0'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

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
      CALL FUNCTION 'ZMM_BUSCA_DADOS_ALGODAO'
        EXPORTING
          i_operacao = '01'.

    WHEN p_opt2.
      CALL FUNCTION 'ZMM_BUSCA_DADOS_ALGODAO'
        EXPORTING
          i_operacao    = '02'
          i_werks       = p_werks
          i_lgort       = p_lgort
          i_charg       = p_charg
          i_nr_romaneio = p_roman.

    WHEN p_opt5.
      CALL FUNCTION 'ZMM_BUSCA_DADOS_ALGODAO'
        EXPORTING
          i_operacao = '03'.

    WHEN p_opt6.
      CALL FUNCTION 'ZMM_BUSCA_DADOS_ALGODAO'
        EXPORTING
          i_operacao = '04'.

    WHEN p_opt3.
      SUBMIT zsdr0152_contingencia
         AND RETURN.

    WHEN p_opt4.
      SUBMIT zsdr0153_contingencia
         AND RETURN.

  ENDCASE.


ENDFORM.

**********************************************************************
**********************************************************************
