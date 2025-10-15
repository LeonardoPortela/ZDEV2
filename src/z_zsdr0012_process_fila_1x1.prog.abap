*&-------------------------------------------------------------------------------------------------------*
*& Método         : Z_ZSDR0012_PROCESS_FILA_1X1                                                          *
*& Chamado        : USER STORY 157683                                                                    *
*& Data           : 13/11/2024                                                                           *
*& Especificado   : Paulo Quevedo                                                                        *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 02/012025|DEVK9A1XAW |NSEGATIN       | RPrograma de execução do JOB da Fila 1x1. Dev. Inicial.        *
*--------------------------------------------------------------------------------------------------------*
REPORT z_zsdr0012_process_fila_1x1.

*--------------------------------------------------------------------*
* C O N S T A N T S                                                  *
*--------------------------------------------------------------------*
CONSTANTS: vl_variant TYPE raldb_vari VALUE 'FILTRO_JOB'.

*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
PARAMETERS: p_times TYPE i DEFAULT 1,
            p_wait  TYPE i DEFAULT 10.

*--------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                *
*--------------------------------------------------------------------*
START-OF-SELECTION.

  IF sy-batch EQ abap_true.
    TRY.
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.

    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.

    ENDIF.

  ENDIF.

  DO p_times TIMES.
    SUBMIT zsdr0012 USING SELECTION-SET vl_variant
                          TO SAP-SPOOL
                          WITHOUT SPOOL DYNPRO
                          AND RETURN.

    WAIT UP TO p_wait SECONDS.

  ENDDO.
