FUNCTION ZFI_CRIAR_LANCAMENTO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EDIT) TYPE  ZFI_EDIT
*"  EXPORTING
*"     REFERENCE(E_NUMERO_LANCAMENTO) TYPE  ZSEQ_LAC_INS
*"  TABLES
*"      T_026 STRUCTURE  ZFIT0026
*"----------------------------------------------------------------------


PERFORM f_grava_lancamento USING i_edit
                                 t_026[]
                        CHANGING e_numero_lancamento.


ENDFUNCTION.
