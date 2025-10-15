FUNCTION-POOL ZFUNCOES MESSAGE-ID ZFUNCOES.

"Bibliotecas
type-pools: icon.

data: usa_popup type c length 1.

data: ok_code type sy-ucomm,
      ok_sair type sy-ucomm value 'SAIR',
      it_cockpit_catalog type lvc_t_fcat,
      cockpit_gs_layout  type lvc_s_layo.

*TELA 9999**********************************************************
constants: c_x type c length 1 value 'X',
           c_r type c length 1 value 'R',
           c_v type c length 1 value 'V',
           c_e type c length 1 value 'E',
           c_s type c length 1 value 'S',
           c_w type c length 1 value 'W'.

types: begin of ty_mensagens.
        include structure bapiret2.
types:  icons  type char04,
        texto  type string.
types: end of ty_mensagens.

data: it_mensagens type table of ty_mensagens with header line.

data: cockpit_prim_msg      type c length 1,
      cockpit_container_msg type ref to cl_gui_custom_container,
      cockpit_alv_msg       type ref to cl_gui_alv_grid.
********************************************************************
