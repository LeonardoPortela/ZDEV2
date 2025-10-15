PROCESS BEFORE OUTPUT.
 MODULE STATUS_0100.

PROCESS AFTER INPUT.
 MODULE USER_COMMAND_0100.

process on value-request.
   field p_ind_matnr module busca_material.
   field p_ind_werks module busca_centros.
   field p_ind_lgort module busca_deposito.
