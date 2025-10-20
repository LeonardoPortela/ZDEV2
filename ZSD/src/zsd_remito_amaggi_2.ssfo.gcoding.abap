DATA : w_xblnr      TYPE vbrk-xblnr.
DATA : w_it_gen     LIKE LINE OF t_it_gen.
DATA : w_konv       LIKE LINE OF t_konv.
DATA : wa_vbrp      LIKE LINE OF t_vbrp.
DATA : wa_vbap      LIKE LINE OF t_vbap.
DATA : w_vbrk_xblnr TYPE vbrk-xblnr.
DATA : wa_knmt      LIKE LINE OF t_knmt.
DATA : s_vbpa       TYPE vbpa.
DATA : w_kpein      TYPE konv-kpein.
DATA : w_kmein      TYPE konv-kmein.
DATA : w_kumza      TYPE konv-kumza.
DATA : w_vbelv      TYPE vbfa-vbelv.
DATA : w_adrnr      TYPE adrc-addrnumber.
DATA : wa_bsas      TYPE bsas.
DATA : wa_bsis      TYPE bsis.
DATA : wa_zsdyt0049 TYPE zsdyt0049.
DATA : wa_lfa1      TYPE lfa1.



*--------------------------------------------------*
*                   Empresa                        *
*--------------------------------------------------*

*Obtenemos N° sociedad de la empresa
SELECT SINGLE bukrs
  FROM tvko
  INTO w_bukrs
  WHERE vkorg = is_dlv_delnote-hd_org-salesorg.


*Obtenemos N° Dirección de la empresa
SELECT SINGLE adrnr
  INTO w_adrnr
  FROM t001
  WHERE bukrs EQ w_bukrs.

*Obtenemos todo lo correspondiente
* a ese N° de Dirección.
SELECT SINGLE *
  INTO st_adrc
  FROM adrc
  WHERE addrnumber EQ w_adrnr.

* Obtenemos el Pais.
SELECT SINGLE landx
  INTO h_land
  FROM t005t
  WHERE spras EQ 'S'
    AND land1 EQ st_adrc-country.


* Obtenemos IVA
CLEAR w_variable.
SELECT SINGLE paval
  FROM t001z
  INTO w_variable
  WHERE bukrs = w_bukrs
    AND party = 'J1AFTV'.

IF w_variable IS NOT INITIAL.

  SELECT SINGLE text60
    FROM j_1afitpvt
    INTO w_iva
    WHERE j_1afitp = w_variable
      AND spras    = 'S'.

ENDIF.

* Obtenemos CUIT
SELECT SINGLE paval
  FROM t001z
  INTO w_cuit
  WHERE bukrs = w_bukrs
    AND party = 'J1AIDN'.

w_cuit_aux = w_cuit.

IF w_cuit IS NOT INITIAL.

*Armamos el formato correspondiente para el CUIT de la Empresa.
  CONCATENATE w_cuit(2) w_cuit+2(8) w_cuit+10(1)
         INTO w_cuit
         SEPARATED BY '-'.
ENDIF.

* Obtenemos IIBB
CLEAR w_iibb_txt.
SELECT SINGLE paval
  FROM t001z
  INTO w_iibb_txt
  WHERE bukrs = w_bukrs
    AND party = 'J1AGIN'.

IF w_iibb_txt IS NOT INITIAL.

  CONCATENATE w_iibb_txt(3)
              w_iibb_txt+3(6)
              w_iibb_txt+9(1)
         INTO w_iibb_txt
         SEPARATED BY '-'.

ENDIF.

* Obtenemos Inicio de Actividades
CLEAR w_inicio_act.
SELECT SINGLE paval
  FROM t001z
  INTO w_inicio_act
  WHERE bukrs = w_bukrs
    AND party = 'J1AFDT'.

IF w_inicio_act IS NOT INITIAL.
*Sacamos el formato que viene por tabla para el Inicio de Actividades.
  CONCATENATE w_inicio_act(2) w_inicio_act+3(2) w_inicio_act+6(4)
       INTO w_inicio_act.
*Armamos el formato correspondiente para el Inicio de Actividades.
  CONCATENATE w_inicio_act(2) w_inicio_act+2(2) w_inicio_act+4(4)
       INTO w_inicio_act
       SEPARATED BY '/'.
ENDIF.

* Obtenemos el N° de Factura
SELECT SINGLE xblnr vbeln bldat kunnr anzpk vkorg vstel
*  INSERT BCI - 11.08.2025 - 16482
  wadat_ist
  FROM likp
  INTO s_xblnr
  WHERE vbeln EQ is_dlv_delnote-hd_gen-deliv_numb.

w_nro_fac = s_xblnr-xblnr.

*Armamos el formato correspondiente para el Nro de Factura.(ver)
*CONCATENATE w_xblnr+1(4) w_xblnr+6(8)
*INTO w_nro_fac
*SEPARATED BY '-'.

IF s_xblnr IS NOT INITIAL.
*  INSERT BCI - 11.08.2025 - 16482
*Armamos el formato correspondiente para la Fecha de la Factura.
*CONCATENATE s_xblnr-bldat+6(2) s_xblnr-bldat+4(2) s_xblnr-bldat(4)
*       INTO w_fecha
*       SEPARATED BY '/'.

  CONCATENATE s_xblnr-wadat_ist+6(2) s_xblnr-wadat_ist+4(2) s_xblnr-wadat_ist(4)
         INTO w_fecha
         SEPARATED BY '/'.
*  END INSERT BCI - 11.08.2025 - 16482
ENDIF.

*--------------------------------------------------*
*                   Cliente                        *
*--------------------------------------------------*

* Obtenemos datos del Cliente.
IF s_xblnr-kunnr IS NOT INITIAL.

  SELECT SINGLE kunnr regio land1 name1 stras pstlz ort01 ort02 stcd1 fityp
  stcdt
    FROM kna1
    INTO s_kna1
    WHERE kunnr EQ s_xblnr-kunnr.

  IF s_kna1 IS NOT INITIAL.

*Obtenemos descripcion de Region.
    SELECT SINGLE bezei
       FROM t005u
       INTO w_bezei
       WHERE land1 EQ s_kna1-land1
         AND bland EQ s_kna1-regio
         AND spras EQ 'S'.

*Obtenemos descripcion IVA.
    SELECT SINGLE text60
      FROM j_1afitpvt
      INTO w_text60
      WHERE spras    EQ 'S'
        AND j_1afitp EQ s_kna1-fityp.

    w_iva_dest   = w_text60+4.

*Armamos el formato correspondiente para el CUIT.
    CONCATENATE s_kna1-stcd1(2) s_kna1-stcd1+2(8) s_kna1-stcd1+10(1)
           INTO w_cuit_dest
           SEPARATED BY '-'.

*Armamos el formato correspondiente para la Direción del Cliente.
    CONCATENATE s_kna1-stras s_kna1-pstlz s_kna1-ort01 w_bezei
       INTO w_dir
       SEPARATED BY '  '.

  ENDIF.


ENDIF.
*--------------------------------------------------*
*           Domicilio Comercial de Entrega         *                  *
*--------------------------------------------------*

*Obtenemos el numero asociado a la dirección
SELECT SINGLE adrnr
  FROM tvst
  INTO w_adrnr
  WHERE vstel  EQ s_xblnr-vstel.

IF w_adrnr IS NOT INITIAL.

*     Obtenemos la calle, el número, localidad y provoncia
*     correspondiente a la dirección comercial de entrega.
  SELECT SINGLE city1 street
  FROM adrc
  INTO (w_city1, w_street)
  WHERE addrnumber EQ w_adrnr.

ENDIF.


*--------------------------------------------------*
*                Pedido de Ventas                  *
*--------------------------------------------------*

* Obtenemos el número de pedido de ventas.
SELECT SINGLE vbelv
  FROM vbfa
  INTO w_vbeln_p
  WHERE vbeln = s_xblnr-vbeln
  AND   vbtyp_n = 'J'.

IF w_vbeln_p IS NOT INITIAL.
* Obtenemos Código de Material, Descripción de Producto,
* Cantidad y Unidad de Medida.

  SELECT a~vbeln,
         a~matnr,
         a~lfimg,
         a~vrkme,
         a~arktx,
         b~anzpk
    INTO TABLE @t_lips
    FROM lips AS a
    INNER JOIN likp AS b
    ON a~vbeln EQ b~vbeln
    WHERE a~vbeln EQ @s_xblnr-vbeln.

ENDIF.

*--------------------------------------------------*
*                Transporte                        *
*--------------------------------------------------*

* Obtenemos los datos del transporte.
SELECT SINGLE *
  INTO st_zsdt0406
  FROM zsdt0406
  WHERE vbeln EQ s_xblnr-vbeln.

*--------------------------------------------------*
*                C.A.I                             *
*--------------------------------------------------*

* Obtenemos C.A.I y fecha de vencimiento.
SELECT SINGLE j_1apac j_1apacvd
  FROM j_1apacd
  INTO (w_j_1apac, w_j_1apacvd)
  WHERE bukrs = s_xblnr-vkorg
  AND   brnch = s_xblnr-xblnr(4)
  AND   doccls = 'H'
  AND   j_1aprtchr = 'R'.


*Armamos el formato correspondiente para la Fecha de Vencimiento.
*CONCATENATE w_j_1apacvd+6(2) w_j_1apacvd+4(2) w_j_1apacvd(4)
*INTO w_j_1apacvd
*SEPARATED BY '/'.

*--------------------------------------------------*
*               Código de Barras                   *
*--------------------------------------------------*
*Obtenemos el código de barra menos el dígito verificador.
CONCATENATE '091' w_cuit_aux s_xblnr-xblnr(4) s_xblnr-xblnr+5(8)
  INTO w_cod_barras.

*--------------------------------------------------*
*                     COT                          *
*--------------------------------------------------*
*Obtenemos COT
SELECT SINGLE nro_cot
  FROM zsdt0390
  INTO w_nro_cot
  WHERE vbeln = s_xblnr-vbeln.

IF  w_nro_cot IS NOT INITIAL.

  CONCATENATE w_nro_cot s_xblnr-xblnr+5(8)
            INTO w_nro_cot.
ENDIF.
