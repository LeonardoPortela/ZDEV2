* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR175                                                *
* Title.......: Atualiza informações do pedido no COUPA                *
* Date........: 24/03/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr175.

INCLUDE: zmmr175_top,
         zmmr175_scr,
         zmmr175_f01.

START-OF-SELECTION.
  PERFORM: f_seleciona_set,
           f_valida_parametros,
           f_seleciona_dados_bsak,
           f_monta_range_tab_z_bsak,
           f_seleciona_dados_ekbe,
           f_monta_range_tab_z_ekbe,
           f_seleciona_dados_z,
           f_enviar_info_coupa,
           f_exibe_alv.
