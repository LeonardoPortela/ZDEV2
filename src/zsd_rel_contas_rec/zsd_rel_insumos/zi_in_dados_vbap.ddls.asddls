@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Dados de Item da OV processados'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_IN_DADOS_VBAP
  as select from    vbap                 as vbap
    inner join      vbak                 as vbak on vbak.vbeln = vbap.vbeln
    left outer join ZI_IN_OV_ITEM_TRANSF as vbep on  vbep.vbeln = vbap.vbeln
                                                 and vbep.posnr = vbap.posnr
{
  key vbap.vbeln,
  key vbap.posnr,
      vbak.auart,

      case vbak.auart
         when 'ZFUT' then
      //-------------------------------
           case vbep.transferido
             when 'X' then
               cast ( '0' as abap.dec( 15, 2 ) )
             else
               cast ( vbap.zmeng as abap.dec( 15, 2) )  end

         when 'ZTRI' then
      //-------------------------------
           case vbep.transferido
             when 'X' then
               cast ( '0' as abap.dec( 15, 2 ) )
             else
               cast ( vbap.zmeng as abap.dec( 15, 2 ) )  end

         else
      //-------------------------------
           case vbep.transferido
             when 'X' then
               cast ( '0' as abap.dec( 15, 2 ) )
             else
               cast ( vbap.kwmeng as abap.dec( 15, 2 ) )  end

      end                                               as totalq_ov,


      case vbep.transferido when 'X' then
          cast ( '0' as abap.dec( 15, 2 ))
      else
           cast ( vbap.netwr as abap.dec( 15, 2 ))  end as netwr,

      case vbep.transferido when 'X' then
          cast ( '0' as abap.dec( 15, 2 ) )
      else
          cast ( vbap.mwsbp as abap.dec( 15, 2 )) end   as mwsbp    
      
}
