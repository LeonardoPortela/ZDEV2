data: it_screen_status type table of sy-ucomm.

types: begin of ty_saida,
         zcod_log   type ze_opns_cod_log, "A
         sid        type syst_sysid, "B
         instance   type msname2, "C
         data       type sydatum, "D
         hora       type syuzeit, "E
         slgmand    type mandt, "F
         zareasubid type ze_opns_areasubid, "G
         slguser    type rslguser, "H
         slgltrm2   type  rsauterm, "I
         slgtc      type  tcode, "J
         slgrepna   type  program_id, "L
         sal_data   type  char255, "M
         menssagem  type atwrt30, "N
       end of ty_saida.

data: o_alv           type ref to cl_salv_table,
      it_ZTD_OPNS_018 type standard table of ztd_opns_018 initial size 0,
      wa_ZTD_OPNS_018 type zaa005,
      it_saida        type standard table of ty_saida initial size 0,
      wa_saida        type standard table of ty_saida initial size 0,
      t_salv          type standard table of ref to cl_salv_table.
