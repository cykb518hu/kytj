select a.*,b.DiseaseName as projectName from DF_Dataset a
right join [SM_Diseases] b on a.SearchResult_Id=b.ID where b.IsDeleted=0 and b.UserName=@userName
and a.DisplayName like '%'+@dataSetName+'%'
order by a.id desc