TYPES: z_char150(150) type C.

TYPES: ty_informe TYPE STANDARD TABLE OF zfiys_recibo_inf_pagos.

types: begin of y_bseg,
         belnr type bseg-belnr,
         buzei type bseg-buzei,
         WRBTR type bseg-WRBTR,
         umskz type bseg-umskz,
         hkont type bseg-hkont,
         BSCHL type bseg-BSCHL,
       end of y_bseg.

types: begin of y_aux,
     AUGBL TYPE AUGBL,
     IMPORTE TYPE	DMBTR,
       end of y_aux.


TYPES: ty_aux TYPE STANDARD TABLE OF y_aux .
TYPES: ty_bseg TYPE STANDARD TABLE OF y_bseg .

types: begin of y_informes, "ZSTFI_RECIBO_INF_PAGOS
       BELNR type belnr_d,
       GJAHR type gjahr,
       BUZEI type buzei,
       HKONT type hkont,
       ZUONR type dzuonr,
       VALUT type valut,
       WRBTR type WRBTR,
       SHKZG type SHKZG,
       TXT50 type txt50,
       end of y_informes.

TYPES: zgs_informes TYPE y_informes .
TYPES: zgt_informes TYPE STANDARD TABLE OF y_informes .

types: begin of y_informes2, "ZSTFI_RECIBO_INF_PAGOS
       GJAHR type gjahr,
       BELNR type belnr_d,
       BUZEI type buzei,
       HKONT type hkont,
       ZUONR type dzuonr,
       VALUT type valut,
       WRBTR type WRBTR,
       SHKZG type SHKZG,
       TXT50 type txt50,
       end of y_informes2.

TYPES: zgs_informes2 TYPE y_informes2 .
TYPES: zgt_informes2 TYPE STANDARD TABLE OF y_informes2 .
