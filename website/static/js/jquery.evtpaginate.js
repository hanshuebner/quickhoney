// JQuery event-driven pagination plugin
// Written by Mark Perkins, mark@allmarkedup.com
// License: http://unlicense.org/ (i.e. do what you want with it!)

;(function($) {
	
	var defaults = {
		perPage : 5, // number of items per page
		startPage : 1, // page to begin on - NOT zero indexed
		atEnd : 'stop' // loop / stop
	};
	
	$.fn.evtpaginate = function( options )
	{
		return this.each(function(){
		
			var opts = $.extend(true, {}, defaults, options); // set options
			var wrap = opts.wrapper = $(this);
			
			wrap.bind( 'show.evtpaginate', function( e, pageNum ){ show( opts, pageNum-1 ); });
			wrap.bind( 'next.evtpaginate', function(){ next( opts ); });
			wrap.bind( 'prev.evtpaginate', function(){ prev( opts ); });
			wrap.bind( 'first.evtpaginate', function(){ show( opts, 0 ); });
			wrap.bind( 'last.evtpaginate', function(){ show( opts, opts.totalPages-1 ); });
			wrap.bind( 'refresh.evtpaginate', function( e, newopts ){ refresh( opts, newopts ); });
			
			setUp( opts );
		});
	};
	
	function setUp( opts )
	{
		opts.perPage		=	parseInt(opts.perPage);
		opts.items 			=	opts.wrapper.children();
		opts.totalItems		=	opts.items.size();
		opts.totalPages		=	Math.ceil( opts.totalItems / opts.perPage );
		opts.currentPage	=	parseInt(opts.startPage) - 1;
		opts.first 			=	isFirstPage( opts, opts.currentPage );
		opts.last 			=	isLastPage( opts, opts.currentPage );
		opts.pages			=	[];
		
		if ( opts.currentPage > opts.totalPages-1 ) opts.currentPage = opts.totalPages-1;

		opts.items.hide();	
			
		for ( var i = 0; i < opts.totalPages; i++ )
		{
			var startItem = i*opts.perPage;
			opts.pages[i] = opts.items.slice( startItem, (startItem + opts.perPage) );
		}
		
		show( opts, opts.currentPage );
		
		opts.wrapper.trigger( 'initialized.evtpaginate', [opts.currentPage+1, opts.totalPages] );
	}
	
	function refresh( opts, newopts )
	{
		if ( newopts !== undefined ) $.extend(true, opts, newopts); // update options
		opts.startPage = parseInt(opts.currentPage)+1;
		setUp( opts );
	}
	
	function next( opts )
	{
		switch( opts.atEnd )
		{
			case 'loop': show( opts, (opts.last ? 0 : opts.currentPage + 1) ); break;
			default: show( opts, (opts.last ? opts.totalPages - 1 : opts.currentPage + 1) ); break; // stop when getting to last page 
		}
	}
	
	function prev( opts )
	{
		switch( opts.atEnd )
		{
			case 'loop': show( opts, (opts.first ? opts.totalPages - 1 : opts.currentPage - 1) ); break;
			default: show( opts, (opts.first ? 0 : opts.currentPage - 1) ); break; // stop when getting to first page 
		}
	}
	
	function show( opts, pageNum )
	{	
		if ( pageNum > opts.totalPages-1 ) pageNum = opts.totalPages-1;

	    if (opts.pages[opts.currentPage] == undefined) {
		opts.wrapper.trigger('finished.evtpaginate', null, 0, 0);
		return;
	    }
		if ( ! opts.pages[opts.currentPage].is(':animated') )
		{
			opts.wrapper.trigger( 'started.evtpaginate', opts.currentPage+1 );
			
			$.fn.evtpaginate.swapPages( opts, pageNum, function(){
				
				opts.currentPage = pageNum;
				opts.first = isFirstPage( opts, opts.currentPage ) ? true : false;
				opts.last = isLastPage( opts, opts.currentPage ) ? true : false;
				
				opts.wrapper.trigger( 'finished.evtpaginate', [opts.currentPage+1, opts.first, opts.last] );
						
			});
		}
	}
	
	// public, can override this if neccessary
	$.fn.evtpaginate.swapPages = function( opts, pageNum, onFinish )
	{
		opts.pages[opts.currentPage].hide();
		opts.pages[pageNum].show();
		onFinish();
	};
	
	// utility functions
	function isFirstPage( opts, internalPageNum ) { return ( internalPageNum === 0 ); }
	function isLastPage( opts, internalPageNum ) { return ( internalPageNum === opts.totalPages-1 ); }
	
})(jQuery);