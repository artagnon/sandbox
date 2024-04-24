var MusicModel = Backbone.Model.extend({});
var MusicList = Backbone.Collection.extend({ model: MusicModel });

musicList = new MusicList({});
musicList.reset([
    {
	title: 'Setting Forth',
	album: 'Into the Wild',
	artist: 'Eddie Vedder'
    },
    {
	title: 'No Ceiling',
	album: 'Into the Wild',
	artist: 'Eddie Vedder'
    }
]);

var MusicView = Backbone.View.extend({
    tagName: 'div',
    className: 'music entry',
    initialize: function(){
	this.template = _.template('<%= title %> | <%= album %> | <%= artist %>');
    },
    render: function(){
	this.$el.html(this.template(this.model.attributes));
	return this;
    }
});

var MusicFormView = Backbone.View.extend({
    el: 'div.wrapper',
    initialize: function(){
	this.musicListView = new MusicListView({});
    },
    events: {
	'submit #musicform': 'submit'
    },
    submit: function(event){
	/* Post to the server */
	$.post($('#musicform').attr('action'),
	       JSON.stringify($('#musicform').serializeObject()))
	    .done(function(data){
		console.log(data);
	    });

	/* Update the DOM */
	thisModel = new MusicModel($('#musicform').serializeObject());
	this.musicListView.addOne(thisModel);
	return false;
    }
});

var MusicListView = Backbone.View.extend({
    el: '#toEdit',
    collection: musicList,
    addOne: function(thisModel) {
	var musicView = new MusicView({ model: thisModel });
	this.$el.append(musicView.render().el);
    },
    addAll: function() {
	this.collection.forEach(this.addOne, this);
    },
    render: function() {
	this.addAll();
    }
});

var musicApp = new (Backbone.Router.extend({
    routes: { '': 'index' },
    initialize: function(){
	this.musicListView = new MusicListView({});
	this.musicFormView = new MusicFormView({});
    },
    start: function(){
	Backbone.history.start({ pushState: true });
    },
    index: function(){
	this.musicListView.render();
    }
}));

$(function(){
    musicApp.initialize();
    musicApp.start();
});
