-module(copyfiles).
-export([copy_dir_contents/3,copy_file/2]).
-include_lib("kernel/include/file.hrl").

-spec copy_dir_contents(SrcDir::list(),DstDir::list(),{recursive,boolean()}) -> ok.

copy_dir_contents(SrcDir,DstDir,{recursive,Recursive}) ->
    ct_logs:tc_pal(file_copy,"Current Working directory~n~s",[filename:absname(".")]),
    case file:make_dir(DstDir) of
	ok -> ok;
	{error,eexist} -> ok
    end,
    {ok,Files}=file:list_dir(SrcDir),
    copy_filelist(Files,SrcDir,DstDir,Recursive).

copy_filelist([],_,_,_) ->
    ok;
copy_filelist([H|T],SrcDir,DstDir,Recursive) ->
    Pn = filename:join([SrcDir,H]),
    {ok,FID} = file:read_file_info(Pn),
    T1 = FID#file_info.type,
    Type = 
	case T1 of
	    directory ->
		case file:read_link(Pn) of
		    {ok,LinkDir} -> {symdir,LinkDir};
		    {error,einval} -> directory
		end;
	    Any -> Any
	end,
    copy_head(Type,Recursive,Pn,DstDir),
    copy_filelist(T,SrcDir,DstDir,Recursive).

copy_head(regular,_,SrcPath,DstDir) ->
    %% If the head of the filelist is a regular file copy it
    DstFile = filename:join(DstDir,filename:basename(SrcPath)),
    copy_file(SrcPath,DstFile);
copy_head(directory,false,_,_) ->
    %% If doing a non recursive copy, and the current file is a directory
    %% do nothing.
    ok;
copy_head(directory,true,Dirname,DstDir) ->
    %% If doing a recursive copy, and the current file is a directory
    %% do a recursive call to copy files.  No need to make the directory
    %% copyfiles does that.
    NewDst = filename:join(DstDir,filename:basename(Dirname)),
    copy_dir_contents(Dirname,NewDst,{recursive,true});
copy_head({symdir,Link},_,SrcPath,DstDir) ->
    DstFile = filename:join(DstDir,filename:basename(SrcPath)),
    file:make_symlink(Link,DstFile).

copy_file(Src,Dst) ->
    ct_logs:tc_pal(file_copy,"~s ~s",[Src,Dst]),
    {ok,_}=file:copy(Src,Dst),
    {ok,Fi}=file:read_file_info(Src),
    ok=file:write_file_info(Dst,Fi).
   
