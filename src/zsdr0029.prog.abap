*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZSDR0029                                                                             *
*& Chamado        : USER STORY 160135                                                                    *
*& Data           : 13/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 04/12/2024|DEVK9A1XAW |NSEGATIN       | Processamento de Cancelamento de Retorno Manual. Dev inicial  *
*--------------------------------------------------------------------------------------------------------*
REPORT zsdr0029.

*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
* Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_docnum TYPE j_1bdocnum.
  SELECTION-SCREEN SKIP.
* Tipo de processamento
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_valida TYPE c AS CHECKBOX DEFAULT 'X' MODIF ID val,
                p_cancel TYPE c AS CHECKBOX.

  SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
* A T - S E L E C T I O N  S C R E E N                               *
*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Processamento da tela antes de inserir os dados
  PERFORM zf_process_scr_pbo.

*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
START-OF-SELECTION.
* Processa os dados de cancelamento de retorno.
  PERFORM zf_process_data.

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_SCR_PBO
*&---------------------------------------------------------------------*
*       Processamento da tela antes de inserir os dados
*----------------------------------------------------------------------*
FORM zf_process_scr_pbo.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'VAL'.
      screen-input = 0.
      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESS_DATA
*&---------------------------------------------------------------------*
*       Processa os dados de cancelamento de retorno
*----------------------------------------------------------------------*
FORM zf_process_data.

  TABLES j_1bnfdoc.

  CONSTANTS: cl_zv TYPE j_1bnftype           VALUE 'ZV', "Ctg.de nota fiscal
             cl_02 TYPE j_1b_status_fisc_doc VALUE '02'. "Código da situação do documento fiscal

  SELECT SINGLE * FROM j_1bnfdoc WHERE docnum EQ p_docnum.

  IF sy-subrc IS INITIAL.
* Valida Cancelamento Retorno
    IF NOT p_valida IS INITIAL.
      TRY.
* Verificação de impedimento de cancelamento do retorno.
          zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->validar_cancelamento_retorno( EXPORTING i_docnum     = p_docnum
                                                                                                                      i_doc_cancel = p_cancel
                                                                                                            IMPORTING e_erro       = DATA(vl_erro) ).
        CATCH zcx_controle_retorno_rfl INTO DATA(zcxl_controle_rfl).
          zcxl_controle_rfl->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          RETURN.

      ENDTRY.

    ENDIF.
* Executa Cancelamento Retorno
    IF NOT p_cancel IS INITIAL.
      IF j_1bnfdoc-cancel  NE abap_on OR "Estornado = 'X'
         j_1bnfdoc-nftype  NE cl_zv   OR "Devolução NF-e de saída (entrada)
         j_1bnfdoc-cod_sit NE cl_02.     "Documento cancelado
* Não é possível cancelar documento
        MESSAGE s084(lcm_document) DISPLAY LIKE sy-abcde+4(1).
        RETURN.

      ENDIF.
* Atualização das tabelas de reversão (estorno) da NF Retorno
      zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->atualizar_tabelas_z( EXPORTING i_docnum = p_docnum ).
* A nota fiscal foi cancelada
      MESSAGE s023(itmf1).
      LEAVE LIST-PROCESSING.

    ELSE.
* Nota com saldo suficiente para cancelar. NF Nº:
      MESSAGE s398(00) WITH text-003 p_docnum.
      LEAVE LIST-PROCESSING.

    ENDIF.

  ELSE.
* sy-abcde+4(1) = E - Erro.
* Nota fiscal & não foi encontrada
    MESSAGE s106(8b) DISPLAY LIKE sy-abcde+4(1) WITH p_docnum.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.
