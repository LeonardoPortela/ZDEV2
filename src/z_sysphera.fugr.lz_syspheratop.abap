FUNCTION-POOL z_sysphera.                   "MESSAGE-ID ..


TYPES: BEGIN OF type_kob1.
         INCLUDE STRUCTURE zsys_inves.
       TYPES: END OF type_kob1.

DATA: gt_kob1 TYPE TABLE OF type_kob1,
      gs_kob1 TYPE type_kob1.

TYPES: BEGIN OF type_kob2.
         INCLUDE STRUCTURE zsys_inves.
       TYPES: END OF type_kob2.

DATA: gt_kob2 TYPE TABLE OF type_kob2,
      gs_kob2 TYPE type_kob2.

TYPES: BEGIN OF type_imak,
         posnr  TYPE imak-posnr,
         abukrs TYPE imak-abukrs,
         vkostl TYPE imak-vkostl,
         werks  TYPE imak-werks,
         vkokrs TYPE imak-vkokrs,
         vgsber TYPE imak-vgsber,
       END OF type_imak.

DATA: gt_imak TYPE TABLE OF type_imak,
      gs_imak TYPE type_imak.

TYPES: BEGIN OF type_imakz,
         posnr TYPE imakz-posnr,
         objnr TYPE imakz-objnr,
       END OF type_imakz.

DATA: gt_imakz TYPE TABLE OF type_imakz,
      gs_imakz TYPE type_imakz.

* INCLUDE LZ_SYSPHERAD...                    " Local class definition


  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

  DATA: lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.
