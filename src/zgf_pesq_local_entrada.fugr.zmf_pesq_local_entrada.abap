FUNCTION ZMF_PESQ_LOCAL_ENTRADA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VSTEL) TYPE  VSTEL
*"     REFERENCE(I_GRUPO) TYPE  MATKL OPTIONAL
*"     REFERENCE(I_FUNDO) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_ID_LOCAL_ENTREGA) TYPE  ZDE_ID_LOCAL_ENTREGA
*"     REFERENCE(E_DS_LOCAL_ENTREGA) TYPE  ZDE_DS_LOCAL_ENTREGA
*"----------------------------------------------------------------------

                                                            "ZSDT0001LE

  CLEAR: E_ID_LOCAL_ENTREGA,
         E_DS_LOCAL_ENTREGA.

  PERFORM PESQ_LOCAL_ESTOQUE USING I_FUNDO I_VSTEL I_GRUPO CHANGING E_ID_LOCAL_ENTREGA E_DS_LOCAL_ENTREGA.

ENDFUNCTION.
