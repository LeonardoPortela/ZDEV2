*&--------------------------------------------------------------------&*
*&                     Programa Módulo - FI                           &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 06/09/2024                                              &*
*& Descrição: Mapa de Comprovação de Investimentos                    &*
*& Transação: Relatóio para JOB                                       &*
*&--------------------------------------------------------------------&*
*& Projeto  : Ninjas Evolution                                        &*
*& Código Espec.Funcional/Técnica: 74935                              &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
FUNCTION zfi_prot_mcomp_inv.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IV_AREA) TYPE  KOKRS DEFAULT 'MAGI'
*"     VALUE(IV_PERIO) TYPE  MONAT OPTIONAL
*"     VALUE(IV_ANO) TYPE  GJAHR OPTIONAL
*"     VALUE(IV_JOB) TYPE  BTCJOB
*"  EXPORTING
*"     VALUE(EV_ERROR) TYPE  CHAR50
*"     VALUE(EV_RET) TYPE  CHAR1
*"----------------------------------------------------------------------

*======================================================================*
* Lógica Principal
*======================================================================*

  DATA: it_ordens TYPE TABLE OF aufnr.

  TRY.

      DATA(go) = NEW zcl_int_ib_fi_prot_mcomp_inv( ).

      go->set( i_area    = iv_area
               i_perio   = iv_perio
               i_ano     = iv_ano
               it_ordens = it_ordens[] ).

      go->create_job( EXPORTING iv_job = iv_job
                      IMPORTING ev_ret = ev_ret ).

    CATCH cx_root.
      IF sy-batch IS INITIAL.
        MESSAGE 'Erro no processamento!'(e01) TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        ev_error = 'Erro no processamento!'(e01).
      ENDIF.
  ENDTRY.

ENDFUNCTION.
