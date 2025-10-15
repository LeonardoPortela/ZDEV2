*break HFACELLO.
*
*  select single k~lifnr
*         k~name1
*         k~stcd1
*         a~STREET
*    into ( G_CORR_LIFNR
*         , G_CORR_NAME1
*         , G_CORR_STCD1
*         , G_CORR_STREET )
*     from zsdyt0049 as z
*    inner join lfa1 as k on z~lifnr eq k~lifnr
*    inner join adrc as a on a~ADDRNUMBER eq k~adrnr
*    where z~DOC_FAT eq IS_BIL_INVOICE-HD_GEN-BIL_NUMBER.






















