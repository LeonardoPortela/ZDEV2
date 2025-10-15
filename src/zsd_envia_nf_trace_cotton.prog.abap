*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 24.11.2022                                              *
* Descrição: Atualizar NFe Agriq                                     *
* Report   : ZSD_ATUALIZA_NF_AGRIQ                                   *
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*

  DATA(l_task_trace) = 'ENVIA_NF_TRACE_COTTON' && ls_acttab-docnum.

  CALL FUNCTION 'ZSD_ENVIA_NF_TRACE_COTTON' STARTING NEW TASK l_task_trace
    EXPORTING
      i_acttab = ls_acttab.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
