function z_geraldoteste.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(REGISTRO) TYPE  STRING OPTIONAL
*"     REFERENCE(FUNCAO) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------
DATA teste TYPE c.
*DATA: wa TYPE indx.
*  data: wa_bcteste type zbcteste.
*
*  clear wa_bcteste.
*  wa_bcteste-funcao    = funcao.
*  wa_bcteste-dt_atu    = sy-datum.
*  wa_bcteste-hr_atu    = sy-uzeit.
*  if ( registro is initial ).
*    concatenate sy-uname sy-tcode sy-ucomm
*    into wa_bcteste-importing separated by space.
*  else.
*    wa_bcteste-importing = registro.
*  endif.
*
*  modify zbcteste from wa_bcteste.

FREE MEMORY ID 'SIG'.
CALL FUNCTION 'Z_TEST_BTE_00103520'
* EXPORTING
*   I_BSID          =
*   I_BUKRS         =
*   I_KUNNR         =
 IMPORTING
   test = teste
*   E_T_BELNR       =
          .

    WAIT UP TO 7 SECONDS.

*import teste from database indx(zw) id 'SIG'.

    IF teste EQ abap_true.
      MESSAGE 'Fornecedor não foi expandido para nenhuma empresa, não será enviado para o SIGAM' TYPE 'I'.
    ENDIF.

endfunction.
