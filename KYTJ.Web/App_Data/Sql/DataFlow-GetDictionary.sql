select dd.Id ,dd.Code,dd.Name,dd2.Id as LinkedId, dd2.Name as LinkedName,dd2.code as LinkedCode from DIC_Dictionary dd left join 
DIC_DictionaryKind ddk  on dd.Kind_Id =ddk.Id 
left join DIC_KeyValueRelation dkvr on dd.Id =dkvr.Parent_Id 
left join DIC_Dictionary dd2 on dkvr.Value_Id =dd2.Id 
where ddk.Code =@type
and dd.IsDeleted =0