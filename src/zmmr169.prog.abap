* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR169                                                *
* Title.......: Envio da taxa de cambio para o COUPA                   *
* Author......: Gustavo Renato de Lima Nobel                           *
* Date........: 09/02/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr169.

TABLES: tcurr.

DATA: git_tcurr          TYPE TABLE OF tcurr,
      git_zintegrcoupa01 TYPE TABLE OF zintegrcoupa01.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_data TYPE sy-datum OBLIGATORY.
SELECT-OPTIONS: s_fcurr FOR tcurr-fcurr,
                s_moeda FOR tcurr-tcurr.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM f_preenche_tela_selecao.

START-OF-SELECTION.
  PERFORM f_seleciona_informacoes.
  PERFORM f_processa_registros.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_preenche_tela_selecao.

  p_data = sy-datum.

  s_fcurr = 'IEQ'.
  s_fcurr-low = 'BRL'.
  APPEND s_fcurr.

  s_moeda     = 'IEQ'.
  s_moeda-low = 'EUR'.
  APPEND s_moeda.

  s_moeda-low = 'GBP'.
  APPEND s_moeda.

  s_moeda-low = 'USD'.
  APPEND s_moeda.

  s_moeda-low = 'CHF'.
  APPEND s_moeda.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_INFORMACOES
*&---------------------------------------------------------------------*
FORM f_seleciona_informacoes.

  DATA: lva_data_anterior           TYPE sy-datum,
        lva_data_anterior_convetida TYPE char10,
        lva_data_atual              TYPE sy-datum,
        lva_data_atual_convertida   TYPE char10.

  IF sy-batch IS NOT INITIAL.
    lva_data_atual = sy-datum.
  ELSE.
    lva_data_atual = p_data.
  ENDIF.

  "Calcula um dia a menos
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lva_data_atual
      days      = 1
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lva_data_anterior.

  "converter data input.
  lva_data_anterior_convetida = lva_data_anterior+6(2) &&
                                lva_data_anterior+4(2) &&
                                lva_data_anterior(4).
  PERFORM f_data_input_output USING abap_true
                           CHANGING lva_data_anterior_convetida.

  lva_data_atual_convertida = lva_data_atual+6(2) &&
                              lva_data_atual+4(2) &&
                              lva_data_atual(4).
  PERFORM f_data_input_output USING abap_true
                           CHANGING lva_data_atual_convertida.

  " tabela base da lógica.
  SELECT *
    FROM tcurr
    INTO TABLE git_tcurr
    WHERE fcurr IN s_fcurr
      AND tcurr IN s_moeda
      AND gdatu IN (lva_data_anterior_convetida, lva_data_atual_convertida ).
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Erro: não existe registros para o filtro selecionado' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INPUT_OUTPUT
*&---------------------------------------------------------------------*
FORM f_data_input_output USING i_input
                      CHANGING c_data.

  IF i_input IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = c_data
      IMPORTING
        output = c_data.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
      EXPORTING
        input  = c_data
      IMPORTING
        output = c_data.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_INFORMACOES
*&---------------------------------------------------------------------*
FORM f_processa_registros.

  DATA: lva_data     TYPE char10.

  DATA: lwa_taxa_cambio    TYPE zmms_dados_tx_cambio_coupa,
        lwa_zintegrcoupa01 TYPE zintegrcoupa01.

  LOOP AT git_tcurr ASSIGNING FIELD-SYMBOL(<lfs_tcurr>).

    TRY.
        CLEAR: lwa_taxa_cambio, lva_data.

        lva_data = <lfs_tcurr>-gdatu.

        PERFORM f_data_input_output USING abap_false
                                 CHANGING lva_data.

        lva_data = lva_data+6(4) && '-' && lva_data+3(2) && '-' && lva_data(2).

        lwa_taxa_cambio-data_taxa_cambio = lva_data.
        lwa_taxa_cambio-moeda_procedente = <lfs_tcurr>-fcurr.
        lwa_taxa_cambio-moeda_destino    = <lfs_tcurr>-tcurr.
        lwa_taxa_cambio-taxa_cambio      = <lfs_tcurr>-ukurs.

        IF lwa_taxa_cambio-taxa_cambio < 0.
          lwa_taxa_cambio-taxa_cambio = lwa_taxa_cambio-taxa_cambio * -1.
        ENDIF.

        DATA(lo_tx_cambio) = NEW zcl_integracao_tx_cambio_coupa( i_taxa_cambio = lwa_taxa_cambio ).

        IF lo_tx_cambio IS BOUND.
          lo_tx_cambio->zif_integracao_tx_camb_coupa~get_xml( IMPORTING e_xml = DATA(lva_xml) ).

          lo_tx_cambio->zif_integracao_tx_camb_coupa~set_ds_url(
          )->set_ds_data( i_xml = lva_xml
          )->set_send_msg( IMPORTING e_id_integracao = DATA(id_integracao)
                                     e_integracao    = DATA(e_integracao) ).

          lo_tx_cambio->zif_integracao_tx_camb_coupa~get_retorno( IMPORTING e_retorno = DATA(e_retorno) ).
        ENDIF.

        GET TIME.

        lwa_zintegrcoupa01-id_integr  = <lfs_tcurr>-gdatu.
        lwa_zintegrcoupa01-ident_proc	=	'TC'.
        lwa_zintegrcoupa01-dt_atual	  =	sy-datum.
        lwa_zintegrcoupa01-hr_atual	  =	sy-uzeit.
        lwa_zintegrcoupa01-status     = e_retorno.

        MODIFY zintegrcoupa01 FROM lwa_zintegrcoupa01.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

      CATCH zcx_error INTO DATA(ex_erro).
        CLEAR lwa_zintegrcoupa01.

        GET TIME.

        lwa_zintegrcoupa01-id_integr  = <lfs_tcurr>-gdatu.
        lwa_zintegrcoupa01-ident_proc	=	'TC'.
        lwa_zintegrcoupa01-dt_atual	  =	sy-datum.
        lwa_zintegrcoupa01-hr_atual	  =	sy-uzeit.

        MODIFY zintegrcoupa01 FROM lwa_zintegrcoupa01.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        MESSAGE 'Erro: não foi possível cadastrar a taxa de cambio' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    FREE: lo_tx_cambio.

  ENDLOOP.

ENDFORM.
