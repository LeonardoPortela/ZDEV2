*&---------------------------------------------------------------------*
*&  Include           ZFIY0013_TOP
*&---------------------------------------------------------------------*
TABLES: bkpf, t685.


*=====================================================
*           *** DEFINICION DE DATOS ***
*=====================================================


DATA: BEGIN OF i_archivo OCCURS 0,
      tipop(1),               "tipo operacion
      fecre(10),              "Fecha de retencion
      incpret(3),             "Inc por el que retiene
      tipco(2),               "Tipo de comprobante
      letco(1),               "Letra de comprobante
      numco(16),              "Numero de comprobante
      fecco(10),              "Fecha del comprobante
      monco(12),              "Monto del comprobante
      tipdoc(1),              "tipo de documento
      numdoc(11),             "numero de documento
      condib(1),              "cond frente a IIBB
      nroins(10),             "numero inscripcion en IIBB
      sitiva(1),              "Situacion frente a IVA
      marog(1),               "marca inscripcion O grav
      mardr(1),               "marca inscripcion D rel
      impgr(12),              "Importe otros gravemenes
      impiva(12),             "Importe IVA
      basimp(12),             "Base imponible para el calculo
      alicu(5),               "Alicuota
      impdet(12),             "Impuesto determinado
      derinsp(12),            "Derecho registro e Inspec
      montret(12),            "Monto Retenido
      Artcal(3),              "Art Inc para el Calculo
      END OF i_archivo.

DATA: BEGIN OF i_bkpf OCCURS 0,
      bukrs TYPE bkpf-bukrs,
      gjahr TYPE bkpf-gjahr,
      belnr TYPE bkpf-belnr,
      bldat TYPE bkpf-bldat,
      xblnr TYPE bkpf-xblnr,
      blart TYPE bkpf-blart,
      knumv TYPE bkpf-knumv,
      END OF i_bkpf.

DATA: BEGIN OF i_bseg OCCURS 0,
      bukrs TYPE bseg-bukrs,
      gjahr TYPE bseg-gjahr,
      belnr TYPE bseg-belnr,
      kunnr TYPE bseg-kunnr,
      END OF i_bseg.

DATA: BEGIN OF i_bset OCCURS 0,
      bukrs TYPE bset-bukrs,
      gjahr TYPE bset-gjahr,
      belnr TYPE bset-belnr,
      kschl TYPE bset-kschl,
      fwbas TYPE bset-fwbas,
      kbetr TYPE bset-kbetr,
      fwste TYPE bset-fwste,
      hwbas TYPE bset-hwbas,
      hwste TYPE bset-hwste,
      END OF i_bset.

DATA: BEGIN OF i_bset2 OCCURS 0,
      bukrs TYPE bset-bukrs,
      gjahr TYPE bset-gjahr,
      belnr TYPE bset-belnr,
      kschl TYPE bset-kschl,
      fwbas TYPE bset-fwbas,
      kbetr TYPE bset-kbetr,
      fwste TYPE bset-fwste,
      hwste TYPE bset-hwste,
      END OF i_bset2.

DATA: BEGIN OF i_kna1 OCCURS 0,
      kunnr TYPE kna1-kunnr,
      stcd1 TYPE kna1-stcd1,
      fityp TYPE kna1-fityp,
      stcd2 TYPE kna1-stcd2,
      stcdt TYPE kna1-stcdt,
      END OF i_kna1.

DATA: BEGIN OF i_konv OCCURS 0,
      knumv TYPE konv-knumv,
      kposn TYPE konv-kposn,
      kschl TYPE konv-kschl,
      kwert TYPE konv-kwert,
      kbetr TYPE konv-kbetr,
      END OF i_konv.
