FUNCTION-POOL zmmg008.                      "MESSAGE-ID ..

* INCLUDE LZMMG008D...                       " Local class definition
TYPES: BEGIN OF ty_pedido.
TYPES: pedido_nome_01  TYPE t16fd-frgct,
       pedido_valor_01 TYPE t16ft-frgxt,
       pedido_nome_02  TYPE t16fd-frgct,
       pedido_valor_02 TYPE t16ft-frgxt,
       pedido_nome_03  TYPE t16fd-frgct,
       pedido_valor_03 TYPE t16ft-frgxt,
       pedido_nome_04  TYPE t16fd-frgct,
       pedido_valor_04 TYPE t16ft-frgxt,
       pedido_nome_05  TYPE t16fd-frgct,
       pedido_valor_05 TYPE t16ft-frgxt,
       pedido_nome_06  TYPE t16fd-frgct,
       pedido_valor_06 TYPE t16ft-frgxt,
       pedido_nome_07  TYPE t16fd-frgct,
       pedido_valor_07 TYPE t16ft-frgxt,
       pedido_nome_08  TYPE t16fd-frgct,
       pedido_valor_08 TYPE t16ft-frgxt.
TYPES: END OF ty_pedido.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA: it_ausp       TYPE TABLE OF ausp WITH HEADER LINE,
      it_ausp_2     TYPE TABLE OF ausp,
      it_ausp_2_aux TYPE TABLE OF ausp,
      it_ausp_3     TYPE TABLE OF ausp,
      it_ausp_4     TYPE TABLE OF ausp,
      it_ausp_4_aux TYPE TABLE OF ausp,
      it_t16fs      TYPE TABLE OF t16fs,
      it_t16fc      TYPE TABLE OF t16fc,
      it_t16fd      TYPE TABLE OF t16fd,
      it_csks       TYPE TABLE OF csks,
      it_t001w      TYPE TABLE OF t001w,
      it_t001       TYPE TABLE OF t001,
      it_cskt       TYPE TABLE OF cskt,
      it_tka01      TYPE TABLE OF tka01,
      it_t16ft      TYPE TABLE OF t16ft,
      it_zmmt0003   TYPE TABLE OF zmmt0003,
      it_user_addr  TYPE TABLE OF user_addr,
      it_t16ft_aux  TYPE TABLE OF t16ft.

DATA: s_werks TYPE RANGE OF werks_d,
      s_kostl TYPE RANGE OF kostl,
      s_tipo  TYPE RANGE OF atwrt.

DATA: vg_atinn LIKE ausp-atinn.

*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: wa_zmmt0003  TYPE zmmt0003,
      wa_t16fs     TYPE t16fs,
      wa_t16ft     TYPE t16ft,
      wa_t001      TYPE t001,
      wa_t001w     TYPE t001w,
      wa_ped       TYPE ty_pedido,
      wa_saida     TYPE zmme0111,
      wa_saida_aux TYPE zmme0111,
      wa_cskt      TYPE cskt,
      wa_csks      TYPE csks,
      wa_user_addr TYPE user_addr,
      wa_ausp_2    TYPE ausp,
      wa_ausp_3    TYPE ausp,
      wa_t16fd     TYPE t16fd.
