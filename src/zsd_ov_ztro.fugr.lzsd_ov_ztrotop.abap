FUNCTION-POOL zsd_ov_ztro.                  "MESSAGE-ID ..

TYPES: BEGIN OF type_vttp,
         tknum TYPE vttp-tknum,
         tpnum TYPE vttp-tpnum,
         vbeln TYPE vttp-vbeln,
       END   OF type_vttp,

       BEGIN OF type_lips,
         vbeln TYPE lips-vbeln,
         posnr TYPE lips-posnr,
         vgbel TYPE lips-vgbel,
         vgpos TYPE lips-vgpos,
         ntgew TYPE lips-ntgew,
         brgew TYPE lips-brgew,
         gewei TYPE lips-gewei,
       END   OF type_lips,

       BEGIN OF type_vfkp,
         fknum TYPE vfkp-fknum,
         fkpos TYPE vfkp-fkpos,
         bukrs TYPE vfkp-bukrs,
         refty TYPE vfkp-refty,
         rebel TYPE vfkp-rebel,
         repos TYPE vfkp-repos,
         kzwi1 TYPE vfkp-kzwi1,
         knumv TYPE vfkp-knumv,
       END   OF type_vfkp.

DATA: t_vttp     TYPE TABLE OF type_vttp,
      t_lips     TYPE TABLE OF lips,
      t_ekbe     TYPE TABLE OF ekbe,
      t_ekko     TYPE TABLE OF ekko,
      t_likp     TYPE TABLE OF likp,
      t_vbak     TYPE TABLE OF vbak,
      t_vbap     TYPE TABLE OF vbap,
      t_vfkp     TYPE TABLE OF type_vfkp,
      t_vbkd     TYPE TABLE OF vbkd,
      t_vtpa     TYPE TABLE OF vtpa,
      v_vbeln    TYPE vbak-vbeln,
      v_vbeln_a  TYPE vbak-vbeln,
      v_fat      TYPE vbak-vbeln,
      t_bapiret2 TYPE TABLE OF bapiret2,
      t_bdc      TYPE TABLE OF bdcdata,
      t_marc     TYPE TABLE OF marc,
      t_mvke     TYPE TABLE OF mvke,
      t_zsdt0022 TYPE TABLE OF zsdt0022,
      s_vttk     TYPE vttk,
      t_tvkwz_f  TYPE TABLE OF tvkwz,
      s_tvkwz_f  TYPE tvkwz,
      t_tvkwz_r  TYPE TABLE OF tvkwz,
      s_tvkwz_r  TYPE tvkwz,
      t_konv     TYPE TABLE OF konv,
      s_konv     TYPE konv.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim
