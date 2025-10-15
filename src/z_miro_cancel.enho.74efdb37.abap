"Name: \FU:BAPI_INCOMINGINVOICE_CANCEL\SE:BEGIN\EI
ENHANCEMENT 0 Z_MIRO_CANCEL.
*
*  DATA: v_miro(14),
*        v_ano(4).
*
*  v_ano = fiscalyear.
*
*  CONCATENATE invoicedocnumber v_ano into v_miro.
*  export v_miro to memory id 'MIRO_CANCEL'.

ENDENHANCEMENT.
