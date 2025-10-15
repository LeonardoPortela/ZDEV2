*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 25.11.2022                                              &*
*& Descrição: Processamento Baixar dados receitas                     &*
*&--------------------------------------------------------------------&*
*& Projeto  : Algodão                                                 &*
*&--------------------------------------------------------------------&*
REPORT zsdr0141_job MESSAGE-ID zjob.

************************************************************************
*& variaveis globais
************************************************************************
DATA: t_status     TYPE zde_btcstatus_t,
      e_quantidade TYPE i.

************************************************************************
*  parametro ID_REFERENCIA
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
PARAMETERS     : p_data  TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
*&      Start-Of-Selection
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*---------------------------------------------
*-Processamento
*---------------------------------------------
  PERFORM f_processa_dados.

************************************************************************
*&-processamento dos fardos
************************************************************************
FORM f_processa_dados.

*---------------------------------
* baixar receitas
*---------------------------------
  TRY .
      zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
         )->set_baixar_receitas_conta( EXPORTING i_data_inicio  = p_data ).

    CATCH zcx_integracao INTO DATA(ex_integra).
      MESSAGE s024(sd) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.

    CATCH zcx_error      INTO DATA(ex_error).    "  "
      MESSAGE s024(sd) WITH 'Não foram encontradas Receitas para atualização.' DISPLAY LIKE 'W'.
  ENDTRY.

ENDFORM.

************************************************************************
************************************************************************
