*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 10.05.2024                                              *
* Descrição: Ajusta campo TBAMB na J_BNFE_ACTIVE                     *
* Report   : ZLES_AJUSTA_TIPO_AMBIENTE                               *
*--------------------------------------------------------------------*
*--------------------------------------------------------------------*

  DATA: w_cust3         TYPE j_1bnfe_cust3.

  IF i_acttab-tpamb IS INITIAL AND i_acttab-model = '58'.
    CALL FUNCTION 'J_1BNFE_CUST3_READ'
      EXPORTING
        iv_bukrs       = i_acttab-bukrs
        iv_branch      = i_acttab-branch
        iv_model       = i_acttab-model
      IMPORTING
        es_cust3       = w_cust3
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      i_acttab-tpamb     = w_cust3-tpamb.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
